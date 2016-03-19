
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