cat('reading mild and reshaping mild data...')
 mild[,code:=paste0(year(as.Date(deb_sem)),"_",week(as.Date(deb_sem)))]
 mild=mild[,include,with=F]
 mild=as.data.table(gather(mild,key=sites,value=mild_value,-c(code,deb_sem)))
cat('DONE\n')

if ( input$Cluster_algo=="Total")
{
  cat('calculating proportion of sites that had received intervention in mild by code (date)...')
  Nbsites_received=mild[mild_value==1,length(unique(sites)),by="code"]
  setnames(Nbsites_received,"V1","numerateur")
  Nbsites_actif=mild[,length(unique(sites)),by="code"]
  setnames(Nbsites_actif,"V1","denominateur")
  mild= merge(Nbsites_received,Nbsites_actif,
              by.x="code",by.y="code",all.x=T, all.y=T)
  mild[,mild_value:=100*numerateur/denominateur]
  cat('DONE\n')
} else {
  mild=create_facies(mild)
  
  cat('calculating proportion of sites that had received intervention in mild by code (date) and by facies...')
  Nbsites_received=mild[mild_value==1,length(unique(sites)),by="code,facies"]
  setnames(Nbsites_received,"V1","numerateur")
  Nbsites_actif=mild[,length(unique(sites)),by="code,facies"]
  setnames(Nbsites_actif,"V1","denominateur")
  mild= merge(Nbsites_received,Nbsites_actif,by.x=c("code","facies"),
              by.y=c("code","facies"),all.x=T)
  mild[,mild_value:=100*numerateur/denominateur]
  cat('DONE\n')
}