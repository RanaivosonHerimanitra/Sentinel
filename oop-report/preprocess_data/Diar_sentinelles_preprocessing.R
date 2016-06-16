mydata= preprocessing_disease()[["Diarrhea"]] #Diarrhée
setnames(mydata,"occurence","diarh")
#put Diarrhée fébrile in a temp file before merging
tmp= preprocessing_disease()[["Diarrhée fébrile"]] 
setnames(tmp,"occurence","diarh_feb")
mydata=merge(mydata,tmp[,list(sites,code,diarh_feb)],
             by.x=c("sites","code"),by.y=c("sites","code"))
return(list(diarrhea=mydata))