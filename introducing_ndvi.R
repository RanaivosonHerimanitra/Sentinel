cat('reading and reshaping NDVI data...')

#ndvi=ndvi[,include,with=F]
include_index= match(include, names(ndvi) )
ndvi= ndvi %>% select(include_index) %>% as.data.frame()

ndvi=as.data.table(gather(ndvi,key=sites,value=ndvi_value,-c(code,deb_sem)))
cat('DONE\n')

if ( input$Cluster_algo=="Total")
{
  setkeyv(ndvi,c("code"))
  cat('calculating mean of NDVI index by code (date)...')
  ndvi=ndvi[,mean(ndvi_value,na.rm = T),by="code"];setnames(ndvi,"V1","ndvi_value")
  cat('DONE\n')
} else {
  ndvi=create_facies(ndvi)
  
  setkeyv(ndvi,c("code","facies"))
  cat('calculating mean of NDVI index by code (date) and by facies...')
  ndvi=ndvi[,mean(ndvi_value,na.rm = T),by="code,facies"];setnames(ndvi,"V1","ndvi_value")
  cat('DONE\n')
}
