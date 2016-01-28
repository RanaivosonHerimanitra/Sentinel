cat('reading mild and reshaping mild data...')
mild=as.data.table(as.data.frame(mild))
mild[,code:=paste0(year(as.Date(deb_sem)),"_",week(as.Date(deb_sem)))]
mild=mild[,include,with=F]
mild=as.data.table(gather(mild,key=sites,value=mild_value,-c(code,deb_sem)))
cat('DONE\n')
mild=create_facies(mild)

if ( input$Cluster_algo=="Total")
{
  cat('calculating number of intervention in mild by code (date)...')
  mild=mild[,sum(mild_value,na.rm = T),by="code"];setnames(mild,"V1","mild_value")
  cat('DONE\n')
} else {
  cat('calculating number of intervention in mild by code (date) and by facies...')
  mild=mild[,sum(mild_value,na.rm = T),by="code,facies"];setnames(mild,"V1","mild_value")
  cat('DONE\n')
}