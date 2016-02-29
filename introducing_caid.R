cat('reading caid and reshaping caid data...')

#
if ( ("data.table" %in% class(caid))==F )
{
  include_index= match(include, names(caid) )
  caid= caid %>% select(include_index) %>% data.frame() 
  caid = caid %>% mutate(code=paste0(year(as.Date(deb_sem)),"_",week(as.Date(deb_sem))) )
  
} else {
 caid[,code:=paste0(year(as.Date(deb_sem)),"_",week(as.Date(deb_sem)))]
}
#


#caid=caid[,include,with=F]
caid=data.table(gather(caid,key=sites,value=caid_value,-c(code,deb_sem)))
#caid[,code:=paste0(year(as.Date(deb_sem)),"_",week(as.Date(deb_sem)))]
cat('DONE\n')

if ( input$Cluster_algo=="Total")
 {
  cat('calculating number of intervention in caid by code (date)...')
  setkey(caid,code)
  Nbsites_received=caid[caid_value==1,length(unique(sites)),by="code"]
  setnames(Nbsites_received,"V1","numerateur")
  Nbsites_actif=caid[,length(unique(sites)),by="code"]
  setnames(Nbsites_actif,"V1","denominateur")
  setkey(Nbsites_received,code);setkey(Nbsites_actif,code)
  caid= merge(Nbsites_received,Nbsites_actif,by.x="code",by.y="code",all.x=T)
  caid[,caid_value:=100*numerateur/denominateur]
  cat('DONE\n')
 } else {
  caid=create_facies(caid)
  cat('calculating number of intervention in mild by code (date) and by facies...')
  Nbsites_received=caid[caid_value==1,length(unique(sites)),by="code,facies"]
  setnames(Nbsites_received,"V1","numerateur")
  Nbsites_actif=caid[,length(unique(sites)),by="code,facies"]
  setnames(Nbsites_actif,"V1","denominateur")
  caid= merge(Nbsites_received,Nbsites_actif,
              by.x=c("code","facies"),
              by.y=c("code","facies"),all.x=T)
  caid[,caid_value:=100*numerateur/denominateur]
  cat('DONE\n')
 }