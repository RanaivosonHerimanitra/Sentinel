cat('reading and reshaping NDVI data...')
ndvi=as.data.table(as.data.frame(ndvi))
ndvi=ndvi[,include,with=F]
ndvi=as.data.table(gather(ndvi,key=sites,value=ndvi_value,-c(code,deb_sem)))
cat('DONE\n')
ndvi=create_facies(ndvi)

if ( input$Cluster_algo=="Total")
{
  cat('calculating mean of NDVI index by code (date)...')
  ndvi=ndvi[,mean(ndvi_value,na.rm = T),by="code"];setnames(ndvi,"V1","ndvi_value")
  cat('DONE\n')
} else {
  cat('calculating mean of NDVI index by code (date) and by facies...')
  ndvi=ndvi[,mean(ndvi_value,na.rm = T),by="code,facies"];setnames(ndvi,"V1","ndvi_value")
  cat('DONE\n')
}
