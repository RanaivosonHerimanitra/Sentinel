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
#retrieve unique values:
X=unique(X[,list(mymonth,myyear,occurence)])