if ( input$Cluster_algo=="Total")
{
  setkeyv(pmm,c("code"))
  cat('calculating mean of rainFall by code (date)...')
  pmm=pmm[,mean(pmm_value,na.rm = T),by="code"];setnames(pmm,"V1","pmm_value")
  cat('DONE\n')
} else {
  
  setkeyv(pmm,"code")
  cat('calculating mean of rainFall by code (date) and by facies...')
  pmm=pmm[get(input$Cluster_algo)==1,mean(pmm_value,na.rm = T),by="code"];
  setnames(pmm,"V1","pmm_value")
  cat('DONE\n')
  
  # setkeyv(pmm,c("code","facies"))
  # cat('calculating mean of rainFall by code (date) and by facies...')
  # pmm=pmm[,mean(pmm_value,na.rm = T),by="code,facies"];setnames(pmm,"V1","pmm_value")
  # cat('DONE\n')
  
}
