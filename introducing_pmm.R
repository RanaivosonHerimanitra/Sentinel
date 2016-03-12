# cat('reading pmm and reshaping rainFall data...')
# 
# #pmm=pmm[,include,with=F]
# require(tidyr)
# if (  ("data.table" %in% class(pmm))==F )
# {
#   include_index= match(include,names(pmm)) #introduce dplyr
#   #mydata=mydata[,include,with=F]
#   pmm= pmm %>% select(include_index) %>% data.frame()
#   
# } else {
#   pmm[,code:=paste0(year(as.Date(deb_sem)),"_",week(as.Date(deb_sem)))]
# }
# pmm=data.table(gather(pmm,key=sites,value=pmm_value,-c(code,deb_sem)))
# cat('DONE\n')

if ( input$Cluster_algo=="Total")
{
  setkeyv(pmm,c("code"))
  cat('calculating mean of rainFall by code (date)...')
  pmm=pmm[,mean(pmm_value,na.rm = T),by="code"];setnames(pmm,"V1","pmm_value")
  cat('DONE\n')
} else {
  setkeyv(pmm,c("code","facies"))
  cat('calculating mean of rainFall by code (date) and by facies...')
  pmm=pmm[,mean(pmm_value,na.rm = T),by="code,facies"];setnames(pmm,"V1","pmm_value")
  cat('DONE\n')
}
