# Malaria 
mydata= tdr_malaria(htc=F)
source("preprocessing.R")
mydata=preprocessing_disease(include_all_sites=F)
sentinel=fread("data/sentinel_codes.csv")
setnames(sentinel,c("Centre","Code"),c("name","sites") )
sentinel[,sites:=tolower(sites)]
sentinel_latlong=rbind(sentinel_latlong,
                       sentinel[!(sites %in% sentinel_latlong$sites)],
                       fill=T)
Malaria=mydata[["Malaria"]]
Malaria[,occurence:=sum(occurence,na.rm = T),by="code"]
Malaria=unique(Malaria[,list(deb_sem,code,occurence)],by=NULL)
# ILI 
ili=as.data.table(gather(ili,key=sites,value=Synd_g,-c(code,deb_sem)))
ili[,Synd_g:=sum(Synd_g,na.rm = T),by="code"]
ili=unique(ili[,list(deb_sem,code,Synd_g)],by=NULL)
#return result:
return(list(Malaria=Malaria,ili=ili))