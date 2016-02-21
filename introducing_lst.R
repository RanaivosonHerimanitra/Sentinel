cat('reading temperature and reshaping temperature data...')

#lst=lst[,include,with=F]
include_index= match(include, names(lst) )
lst= lst %>% select(include_index) %>% as.data.frame()

lst=as.data.table(gather(lst,key=sites,value=temperature,-c(code,deb_sem)))
cat('DONE\n')


if ( input$Cluster_algo=="Total")
{
  setkeyv(lst,c("code"))
  cat('calculating mean of temperature by code (date)...')
  lst=lst[,mean(temperature,na.rm = T),by="code"];setnames(lst,"V1","temperature")
  cat('DONE\n')
} else {
  setkeyv(lst,c("code","facies"))
  lst=create_facies(lst)
  cat('calculating mean of temperature by code (date) and by facies...')
  lst=lst[,mean(temperature,na.rm = T),by="code,facies"];setnames(lst,"V1","temperature")
  cat('DONE\n')
}
