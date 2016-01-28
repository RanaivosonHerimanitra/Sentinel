cat('reading temperature and reshaping temperature data...')
lst=as.data.table(as.data.frame(lst))
lst=lst[,include,with=F]
lst=as.data.table(gather(lst,key=sites,value=temperature,-c(code,deb_sem)))
cat('DONE\n')
lst=create_facies(lst)

if ( input$Cluster_algo=="Total")
{
  cat('calculating mean of temperature by code (date)...')
  lst=lst[,mean(temperature,na.rm = T),by="code"];setnames(lst,"V1","temperature")
  cat('DONE\n')
} else {
  cat('calculating mean of temperature by code (date) and by facies...')
  lst=lst[,mean(temperature,na.rm = T),by="code,facies"];setnames(lst,"V1","temperature")
  cat('DONE\n')
}
