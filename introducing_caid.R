cat('reading caid and reshaping caid data...')
caid=as.data.table(as.data.frame(caid))
caid[,code:=paste0(year(as.Date(deb_sem)),"_",week(as.Date(deb_sem)))]
caid=caid[,include,with=F]
caid=as.data.table(gather(caid,key=sites,value=caid_value,-c(code,deb_sem)))
cat('DONE\n')
caid=create_facies(caid)

if ( input$Cluster_algo=="Total")
{
  cat('calculating number of intervention in caid by code (date)...')
  caid=caid[,sum(caid_value,na.rm = T),by="code"];setnames(caid,"V1","caid_value")
  cat('DONE\n')
} else {
  cat('calculating number of intervention in mild by code (date) and by facies...')
  caid=caid[,sum(caid_value,na.rm = T),by="code,facies"];setnames(caid,"V1","caid_value")
  cat('DONE\n')
}