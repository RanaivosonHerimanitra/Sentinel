calculate_minsan=function(data=mydata,
                          slope_minsan=input$slope_minsan,
                          year_choice=year(Sys.Date()),
                          week_choice=ifelse(Sys.Date()-as.Date(paste0(year(Sys.Date()),"-01-01"))<8
                                             ,1,week(Sys.Date())),
                          minsan_weekrange=input$minsan_weekrange,
                          minsan_consecutive_week=input$minsan_consecutive_week,
                          byvar="code") 
  {
  
  
  if ( as.numeric(week_choice)== 1 )
  {
    year_choice= as.numeric(year_choice)-1
  }
 
  max_deb_sem= max(as.Date(data$deb_sem))

  
  
    cat('chosen slope parameter is:',slope_minsan,'\n')
    cat('weeks following this slope parameter:',minsan_weekrange,'\n')
    cat('Are these weeks consecutive? :',minsan_consecutive_week,'\n')
    
    S0=data[as.Date(deb_sem)==max_deb_sem,list(code,sites,occurence)]
    S1=data[as.Date(deb_sem)==max_deb_sem-7,list(sites,occurence)]
    setnames(S1,"occurence","occurence_1")
    cat('merging data for MinSan algorithm...\n')
    minsan=merge(S0,S1,by.x="sites",by.y="sites")
    S2=data[as.Date(deb_sem)==max_deb_sem-2*7,list(sites,occurence)]
    setnames(S2,"occurence","occurence_2")
    cat('merging data for MinSan algorithm...')
    minsan=merge(minsan,S2,by.x="sites",by.y="sites")
    cat('DONE\n')
    if ( as.numeric(minsan_weekrange) ==4 )
    {
      S3=data[as.Date(deb_sem)==max(as.Date(deb_sem))-3*7,list(sites,occurence)]
      setnames(S3,"occurence","occurence_3")
      minsan=merge(minsan,S3,by.x="sites",by.y="sites")
      cat('merging with PaluConf data...')
      data=merge(data,minsan[,list(sites,occurence_1,occurence_2,occurence_3)],
                 by.x="sites",by.y="sites")
      cat('DONE\n')
     
      cat('detection of alerts per site using MinSan algorithm...\n')
      
      if ( minsan_consecutive_week==T) 
      {
        cat('consecutive MinSan calculation for 04 weeks...\n')
        data[,alert_status:=ifelse(occurence>=slope_minsan*occurence_1 &
                                     occurence_1>=slope_minsan*occurence_2 &
                                     occurence_2>=slope_minsan*occurence_3,"alert","normal")]
      } else {
        cat('Non consecutive MinSan calculation for 04 weeks...')
        data[,alert_status:=ifelse(occurence>=slope_minsan*occurence_1 |
                                     occurence>=slope_minsan*occurence_2 |
                                     occurence>=slope_minsan*occurence_3,"alert","normal")]
        cat('DONE\n')
      }
      cat('remove temporary files...')
      rm(S0);rm(S1);rm(S2);rm(S3)
      cat('DONE\n')
      
      
      
    } else {
      cat('merging with PaluConf data...')
      data=merge(data,minsan[,list(sites,occurence_1,occurence_2)],
                 by.x="sites",by.y="sites")
      cat('DONE\n')
      
      cat('remove temporary files...')
      rm(S0);rm(S1);rm(S2)
      cat('DONE\n')
      cat('detection of alerts per site using MinSan algorithm...\n')
      if ( minsan_consecutive_week==T) 
      {
        cat('consecutive MinSan alert detection for 03 weeks...\n')
        cat('names in data are',names(data),'\n')
        data[,alert_status:=ifelse(occurence>=slope_minsan*occurence_1 &
                                     occurence_1>=slope_minsan*occurence_2 
                                   ,"alert","normal")]
      } else {
        cat('Non consecutive MinSan calculation for 03 weeks...\n')
        data[,alert_status:=ifelse(occurence>=slope_minsan*occurence_1 |
                                     occurence>=slope_minsan*occurence_2 
                                   ,"alert","normal")]
      }
    }
  
  
  
  
  cat("Weekly number of sites in alert using MinSan (all)...\n")
  Nbsite_beyond=data[is.na(occurence)==F & alert_status=="alert",
                     length(unique(sites)),by=byvar]
  
  setnames(Nbsite_beyond,"V1","eff_beyond")
  cat("Weekly number of sites that had given data (all)...\n")
  Nbsite_withdata=data[is.na(occurence)==F,length(unique(sites)),by=byvar]
  setnames(Nbsite_withdata,"V1","eff_total")
  
  
  
  cat('MERGE to give proportion of sites beyond n_percentile per week...')
  propsite_alerte_minsan=merge(x=Nbsite_withdata,y=Nbsite_beyond,
                                   by.x=byvar,by.y=byvar,all.x=T)
  cat('DONE\n')
  
  
  cat('calculate prop and change NA to zero...')
  propsite_alerte_minsan[,prop:=ifelse(is.na(eff_beyond/eff_total)==T,0.0,
                                       eff_beyond/eff_total)]
  cat('DONE\n')

  
  
  cat('merge with deb_sem to reorder time series...')
 #
     propsite_alerte_minsan=merge(propsite_alerte_minsan,
                                      data[,list(code,deb_sem,sites,alert_status,East,
                                                 South,High_land,Fringe,excepted_East,
                                                 excepted_High_land)],
                                      by.x="code",by.y="code")
  cat('DONE\n')
  
  
  ###################################by facies ###############################
  cat("Weekly number of sites in alert using MinSan (by facies)...\n")
  Nbsite_beyond=data[is.na(occurence)==F & alert_status=="alert",
                     length(unique(sites)),by=c(byvar,"facies")]
  setnames(Nbsite_beyond,"V1","eff_beyond")
  cat("Weekly number of sites that had given data (by facies)...\n")
  Nbsite_withdata=data[is.na(occurence)==F,length(unique(sites)),by=c(byvar,"facies")]
  setnames(Nbsite_withdata,"V1","eff_total")
  cat('MERGE to give proportion of sites beyond n_percentile per week (by facies)...')
  propsite_alerte_minsan_byfacies=merge(x=Nbsite_withdata,y=Nbsite_beyond,
                               by.x=c(byvar,"facies"),by.y=c(byvar,"facies"),all.x=T)
  cat('DONE\n')
  cat('calculate prop and change NA to zero...')
  propsite_alerte_minsan_byfacies[,prop:=ifelse(is.na(eff_beyond/eff_total)==T,0.0,
                                       eff_beyond/eff_total)]
  cat('DONE\n')
  cat('merge with deb_sem to reorder time series...')
  propsite_alerte_minsan_byfacies=merge(unique(propsite_alerte_minsan_byfacies[,list(code,prop,facies)]),
                               unique(data[,list(code,deb_sem,facies)]),
                               by.x=c(byvar,"facies"),by.y=c(byvar,"facies"))
  cat('DONE\n')
  rm(Nbsite_withdata);rm(Nbsite_beyond);gc()
  
  cat('calculate radius for per site for percentile algorithm alert...')
  #fixons la taille du cercle à 15 pour les alertes
  #la taille des cercles en situation normale est proportionnelle
  # au nombre de cas mais ne dépasse pas 15.
  data[alert_status=="alert",myradius:=15.0]
  data[alert_status=="normal",sum_occurence_week:=sum(occurence,na.rm=T),by="code"]
  data[alert_status=="normal", myradius:=15*occurence/sum_occurence_week,by="sites,code"]
  
  #set a minimum value if less than 2.5 in radius (for visibility purpose):
  data[alert_status=="normal", myradius:=ifelse(myradius<2.5,2.5,myradius),by="sites,code"]
  
  data[alert_status %in% NA | myradius %in% NA , myradius:=5.0]
  
   # data[,nbsite_alerte:=1.0]; data[,nbsite_normal:=1.0]
   # data[alert_status=="alert",nbsite_alerte:=sum(occurence,na.rm = T)*1.0,by="sites,code"]
   # data[alert_status=="normal",nbsite_normal:=sum(occurence,na.rm = T)*1.0,by="sites,code"]
   # data[alert_status=="normal",myradius:=5*(nbsite_alerte+1)/sqrt(nbsite_normal+1)]
   # data[alert_status=="alert",myradius:=5*(nbsite_alerte+1)/sqrt(nbsite_normal+1)]
   # data[alert_status %in% NA, myradius:=10*myradius]
   # 
  cat('DONE\n')
  
  
  return (list(minsan_alerte_currentweek=data[as.Date(deb_sem)==max(as.Date(deb_sem))-7,
                                  list(sites,deb_sem,alert_status,myradius)],
               propsite_alerte_minsan=propsite_alerte_minsan,
               propsite_alerte_minsan_byfacies=propsite_alerte_minsan_byfacies
  ))
  
#   cat('Handling NA values in Minsan computation...')
#   #should be no_data instead of normal
#   data[,alert_status:=ifelse(alert_status %in% NA,"normal",alert_status)]
#   cat('DONE\n')
}