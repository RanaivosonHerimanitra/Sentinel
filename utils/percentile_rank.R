##############################Calculate percentile rank for each point ##############
#function that calculates percentile rank
perc_rank = function(x,x0) {f=ecdf(x);return(round(100*f(x0)) )}

# data to be analyzed and copies to store rank:
Malaria=fread("./data/PaluConf.csv")
Malaria_rank=Malaria[as.numeric(substr(code,1,4))>=2012]
Diarrhea=fread("./data/Diarrh.csv")
Diarrhea_rank=Diarrhea[as.numeric(substr(code,1,4))>=2012]
ILI=fread("./data/ili.csv")
ILI_rank=Diarrhea[as.numeric(substr(code,1,4))>=2012]

# Semaine epidemiologic we need to loop thru
mycode=unique(c(Malaria$code,Diarrhea$code))
# site we need to loop thru:
mysite= names(Malaria)[!(names(Malaria) %in% c("code","deb_sem"))]


get_percentile_rank= function(data=Malaria,
                              output=Malaria_rank,
                              disease.name="Malaria") {
  # loop and compute:
  for ( m in mysite ) {
    #get this site m:
    occurrence_vector= data[is.na(get(m))==F,get(m)]
    for ( j in mycode[as.numeric(substr(mycode,1,4))>=2012] )
    {
      cat("Calculate percentile rank ",disease.name ,",for site ",m,",week ",j,"...")
      p=0
      occurrence_value=data[code==j & is.na(get(m))==F ,get(m)]
      if ( length(occurrence_value)==0 ) {
        output[code==j,(m):=0]
      } else {
        output[code==j,(m):=perc_rank( occurrence_vector, occurrence_value )]
      }
      p=p+1
      cat("DONE\n")
    }
  }
  return(output)
}
Malaria_rank=get_percentile_rank(data=Malaria,output=Malaria_rank,
                                     disease.name="Malaria")
#reshape data:
Malaria_rank=as.data.table(gather(Malaria_rank,key=sites,value=perc_rank,-c(code,deb_sem)))

write.table(Malaria_rank,
            "percentile_rank/Malaria_rank.csv",row.names = F,sep = ";")
################################################################################
Diarrhea_rank=get_percentile_rank(data=Diarrhea,output=Diarrhea_rank,
                                 disease.name="Diarrhea")
#reshape data:
Diarrhea_rank=as.data.table(gather(Diarrhea_rank,key=sites,value=perc_rank,-c(code,deb_sem)))

write.table(Diarrhea_rank,
            "percentile_rank/Diarrhea_rank.csv",row.names = F,sep = ";")

################################################################################
ILI_rank=get_percentile_rank(data=ILI,output=ILI_rank,
                                  disease.name="ILI")
#reshape data:
ILI_rank=as.data.table(gather(ILI_rank,key=sites,value=perc_rank,-c(code,deb_sem)))

write.table(ILI_rank,
            "percentile_rank/ILI_rank.csv",row.names = F,sep = ";")