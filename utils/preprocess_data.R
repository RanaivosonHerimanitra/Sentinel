##############################Preprocessing of data except for PaluCOnf ################
preprocess_data = function (Cluster_algo=input$Cluster_algo)
{
  include<-c("deb_sem","code","abv","abz","ahh","ajb","atb","bel","bhk","boe","bos",
             "cda","die","dri","ejd","far","fns","iho","mae","mdv","mhj","mia","mjr",
             "mnj","mrb","mrg","mrt","mtn","nsb","sbv","stm","tdd","tgr","tlr","toa","tsl")
  
  data=as.data.table(as.data.frame(data))
  data[,code:=paste0(year(as.Date(deb_sem)),"_",week(as.Date(deb_sem)))]
  data=data[,include,with=F]
  data=as.data.table(gather(data,key=sites,value=varname,-c(code,deb_sem)))
  cat('DONE\n')
  data=create_facies(data)
  
  if (Cluster_algo=="Total")
  {
    #cat('calculating number of intervention by code (date)...')
    data=data[,sum(eval(get(varname)),na.rm = T),by="code"];setnames(caid,"V1",varname)
   # cat('DONE\n')
  } else {
    #cat('calculating number of intervention in CAID by code (date) and by facies...')
    data=data[,sum(get(varname),na.rm = T),by="code,facies"];
    setnames(data,"V1",varname)
    #cat('DONE\n')
  }
  return( data )
}