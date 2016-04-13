
if ( input$Cluster_algo=="Total")
{
  setkeyv(ndvi,c("code"))
  cat('calculating mean of NDVI index by code (date)...')
  ndvi=ndvi[,mean(ndvi_value,na.rm = T),by="code"];setnames(ndvi,"V1","ndvi_value")
  cat('DONE\n')
} else {
 
  setkeyv(ndvi,c("code"))
  cat('calculating mean of NDVI index by code (date) and by facies...')
  ndvi=ndvi[get(input$Cluster_algo)==1,mean(ndvi_value,na.rm = T),by="code"];
  setnames(ndvi,"V1","ndvi_value")
  cat('DONE\n')
  
  # ndvi=create_facies(ndvi)
  # setkeyv(ndvi,c("code","facies"))
  # cat('calculating mean of NDVI index by code (date) and by facies...')
  # ndvi=ndvi[,mean(ndvi_value,na.rm = T),by="code,facies"];setnames(ndvi,"V1","ndvi_value")
  # cat('DONE\n')
}
