##############################Prepare data for forecasting ####################
require(forecast)
mae = function (actual,preds)
{
  return ( mean(abs(actual-preds)) )
}
#only select far and mjr
X=PaluConf[,list(code,deb_sem,far,mjr)]
#training length:
#training_length=round(nrow(X)/4)
#spread data
X=as.data.table(gather(PaluConf,key=sites,value=occurence,-c(code,deb_sem)))
setorder(X,deb_sem)
# Farafangana & Mananjary
X=X[sites %in% c("far","mjr")]
#sum per month , per year: 
X[,mymonth:=month(as.Date(deb_sem))]
X[,myyear:=year(as.Date(deb_sem))]
X[,occurence:=sum(occurence,na.rm = T),by="mymonth,myyear"]

if ( input$mymodel=="ARMAX")
{
  X=unique(X[,list(code,deb_sem,occurence,mymonth,myyear)])
  min_semaine=min(X$deb_sem)
  max_semaine=max(X$deb_sem)
  cat("merge with LST...")
    X=merge(X,lst[deb_sem>=min_semaine,list(code,far,mjr)],
            by.x="code",by.y="code",all.x = T)
    setnames(X,old=c("far","mjr"),new=c("lst_far","lst_mjr"))
  cat("DONE\n")
  cat("merge with Rainfall...")
    pmm_tmp= pmm[,list(mean(far,na.rm = T),mean(mjr,na.rm = T)),by="code"]
    setnames(pmm_tmp,"V1","pmm_far");setnames(pmm_tmp,"V2","pmm_mjr")
    pmm_tmp=unique(pmm_tmp[,list(code,pmm_far,pmm_mjr)])
    X=merge(X,pmm_tmp[,list(code,pmm_far,pmm_mjr)],
            by.x="code",by.y="code",all.x = T)
  cat("DONE\n")
  cat("merge with ndvi...")
    X=merge(X,ndvi[deb_sem>=min_semaine,list(code,far,mjr)],
            by.x="code",by.y="code",all.x = T)
    setnames(X,old=c("far","mjr"),new=c("ndvi_far","ndvi_mjr"))
  cat("DONE\n")
  cat("merge with IRS(CAID)...")
    caid[,code:=paste0(substr(deb_sem,1,4),"_",substr(deb_sem,6,7))]
    caid_tmp= caid[,list(sum(far,na.rm = T),sum(mjr,na.rm = T)),by="code"]
    setnames(caid_tmp,"V1","caid_far");setnames(caid_tmp,"V2","caid_mjr")
    caid=unique(caid[,list(code,caid_far,caid_mjr)])
    X=merge(X,caid[,list(code,caid_far,caid_mjr)],
            by.x="code",by.y="code",all.x = T)
  cat("DONE\n")
  cat("merge with MILD...")
    X=merge(X,mild[deb_sem>=min_semaine,list(code,far,mjr)],
            by.x="code",by.y="code",all.x = T)
    setnames(X,old=c("far","mjr"),new=c("mild_far","mild_mjr"))
  cat("DONE\n")
  cat("reoder time series...")
   setorder(X,-deb_sem)
  cat("DONE\n")
} else {
  #retrieve unique values:
  X=unique(X[,list(mymonth,myyear,occurence)])
}