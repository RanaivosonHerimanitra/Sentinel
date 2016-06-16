if ( input$Cluster_algo=="Total")
{
  setkeyv(lst,c("code"))
  cat('calculating mean of temperature by code (date)...')
  lst=lst[,mean(temperature,na.rm = T),by="code"];setnames(lst,"V1","temperature")
  cat('DONE\n')
} else {
  
  setkeyv(lst,"code")
  lst=create_facies(lst)
  cat('calculating mean of temperature by code (date) and by facies...')
  lst=lst[get(input$Cluster_algo)==1,mean(temperature,na.rm = T),by="code"];
  setnames(lst,"V1","temperature")
  cat('DONE\n')
  
 
}
