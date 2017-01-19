#handle week <10 cases:
#@' input must be of class Date
HandleWeek = function(p) {
  MyIsoweek= isoweek(p)
  MyIsoweek= ifelse(MyIsoweek<10,paste0("0",MyIsoweek),MyIsoweek)
  x=paste0(year(p),"_",MyIsoweek)
  return(x)
}
calculate_minsan=function(data=mydata,
                          slope_minsan=input$slope_minsan,
                          year_choice=year(Sys.Date()),
                          week_choice=ifelse(Sys.Date()-as.Date(paste0(year(Sys.Date()),"-01-01"))<8
                                             ,1,isoweek(Sys.Date())),
                          minsan_weekrange=input$minsan_weekrange,
                          minsan_consecutive_week=input$minsan_consecutive_week,
                          byvar="code") 
  {
  #order date by site and handle NA in number of case
  setorder(data,sites,-deb_sem)
  data[is.na(occurence)==T,occurence:=0]
  ##
  if ( as.numeric(week_choice)== 1 )
  {
    year_choice= as.numeric(year_choice)-1
  }
  max_deb_sem= max(as.Date(data$deb_sem))
  #
  max_code= HandleWeek(max_deb_sem)
  CurrentIsoweek =HandleWeek(Sys.Date())
  PrevIsoweek=HandleWeek(Sys.Date()-7)
  
  if (max_code==paste0(year(Sys.Date()),"_",CurrentIsoweek) ) {
    #if we are on the current week, update max_deb_sem
    max_code_prev = paste0(year(Sys.Date()-7),"_",PrevIsoweek)
    max_deb_sem =as.Date(data[code==max_code_prev,get("deb_sem")][1],origin="1970-01-01")
  } 
  
  
    cat('chosen slope parameter is:',slope_minsan,'\n')
    cat('weeks following this slope parameter:',minsan_weekrange,'\n')
    cat('Are these weeks consecutive? :',minsan_consecutive_week,'\n')
    
    cat('merging data for MinSan algorithm...')
      S0=data[as.Date(deb_sem,origin="1970-01-01")==max_deb_sem,list(code,sites,occurence)]
      S1=data[as.Date(deb_sem,origin="1970-01-01")==max_deb_sem-1*7,list(sites,occurence)]
      setnames(S1,"occurence","occurence_1")
      minsan=merge(S0,S1,by.x="sites",by.y="sites")
      S2=data[as.Date(deb_sem,origin="1970-01-01")==max_deb_sem-2*7,list(sites,occurence)]
      setnames(S2,"occurence","occurence_2")
      minsan=merge(minsan,S2,by.x="sites",by.y="sites")
    cat('DONE\n')
    
    
    # 04 consecutive weeks or 03:
    if ( as.numeric(minsan_weekrange) ==4 )
    {
      S3=data[as.Date(deb_sem)==max_deb_sem-3*7,list(sites,occurence)]
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
                                     occurence_2>=slope_minsan*occurence_3 & occurence>0 & occurence_1>0 & occurence_2>0 & occurence_3>0,"alert","normal")]
      } else {
        cat('Non consecutive MinSan calculation for 04 weeks...')
        data[,alert_status:=ifelse( (occurence>=slope_minsan*occurence_1 |
                                     occurence>=slope_minsan*occurence_2 |
                                     occurence>=slope_minsan*occurence_3) & (occurence>0 & occurence_1>0 & occurence_2>0 & occurence_3>0),"alert","normal")]
        cat('DONE\n')
      }
      cat('remove temporary files...')
      rm(S0);rm(S1);rm(S2);rm(S3)
      cat('DONE\n')
      
    } else {
      # 03 weeks:
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
                                     occurence_1>=slope_minsan*occurence_2 & occurence>0 & occurence_1>0 & occurence_2>0 
                                   ,"alert","normal")]
      } else {
        cat('Non consecutive MinSan calculation for 03 weeks...\n')
        data[,alert_status:=ifelse( (occurence>=slope_minsan*occurence_1 |
                                     occurence>=slope_minsan*occurence_2 ) & (occurence>0 & occurence_1>0 & occurence_2>0 )
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
       propsite_alerte_minsan=merge(propsite_alerte_minsan,
                                      data[,list(code,deb_sem,sites,alert_status,East,
                                                 South,High_land,Fringe,excepted_East,
                                                 excepted_High_land)],
                                      by.x="code",by.y="code")
  cat('DONE\n')
  
  #################new method to handle facies ###################################  
  list_facies= c("East","South","High_land","Fringe","excepted_East","excepted_High_land")
  datalist_facies=list()
  for ( f in list_facies)
  {
    cat("Weekly number of sites in alert using MinSan for ",f,"...")
     Nbsite_beyond=data[is.na(occurence)==F & alert_status=="alert" & get(f)==1,
                       length(unique(sites)),by=c(byvar,f)]
     setnames(Nbsite_beyond,"V1","eff_beyond")
    cat("DONE\n")
    cat("Weekly number of sites that had given data for ",f,"...")
     Nbsite_withdata=data[is.na(occurence)==F & get(f)==1,length(unique(sites)),
                         by=c(byvar,f)]
     setnames(Nbsite_withdata,"V1","eff_total")
    cat('DONE\n')
    cat('MERGE to give proportion of sites beyond n_percentile per week for ',f,'...')
    myfacies=merge(x=Nbsite_withdata,y=Nbsite_beyond,
                                     by.x=c(byvar,f),
                                     by.y=c(byvar,f),all.x=T)
    cat('DONE\n')
    cat('calculate prop and change NA to zero...')
    myfacies[,prop:=ifelse(is.na(eff_beyond/eff_total)==T,0.0,
                                                  eff_beyond/eff_total)]
    cat('DONE\n')
    cat('merge with deb_sem to reorder time series...')
    myfacies=merge(unique(myfacies[,list(code,prop)],by=NULL),
                                    unique(data[,list(code,deb_sem)],by=NULL),
                                    by.x=c(byvar),
                                    by.y=c(byvar))
    cat('DONE\n')
    rm(Nbsite_withdata);rm(Nbsite_beyond);gc()
    datalist_facies[[f]]=myfacies
    #append to a single and unique dataframe:
    if ( f==list_facies[1])
    {
      propsite_alerte_minsan_byfacies= datalist_facies[[f]]
      propsite_alerte_minsan_byfacies[,facies:=f]
    } else {
      tmp= datalist_facies[[f]]
      tmp[,facies:=f]
      propsite_alerte_minsan_byfacies=rbind(propsite_alerte_minsan_byfacies,tmp)
      rm(tmp);gc()
    }
    cat("DONE\n")
  }
 
  
##########################################################################
  cat('calculate radius for per site for percentile algorithm alert...')
  # # TODO: en fonction du NB cas
  # #fixons la taille du cercle à 15 pour les alertes
  # #la taille des cercles en situation normale est proportionnelle
  # # au nombre de cas mais ne dépasse pas 15.
  # data[alert_status=="alert",myradius:=15.0]
  # data[alert_status=="normal",sum_occurence_week:=sum(occurence,na.rm=T),by="code"]
  # data[alert_status=="normal", myradius:=15*occurence/sum_occurence_week,by="sites,code"]
  # #set a minimum value if less than 2.5 in radius (for visibility purpose):
  # data[alert_status=="normal" & is.na(myradius)==F, myradius:=ifelse(myradius<2.5,2.5,myradius),by="sites,code"]
  # data[alert_status %in% NA | myradius %in% NA , myradius:=5.0]
  data[,myradius:=sum(occurence,na.rm = T)+1,by="code,sites"]
   
  cat('DONE\n')
  
  #if last week is current week then substract
  #otherwise keep!
  if (max_code==paste0(year(Sys.Date()),"_",isoweek(Sys.Date())) ) {
    #if max_date == current week then exclude this current week
    #from calculation of alert , otherwise include 
    mycode=paste0(year(Sys.Date()-7),"_",isoweek(Sys.Date()-7))
    minsan_alerte_currentweek=data[code==mycode,list(sites,deb_sem,alert_status,myradius)]
  } else {
    minsan_alerte_currentweek=data[code==max_code,list(sites,deb_sem,alert_status,myradius)]
  }
  
  #convert to usual date format
  minsan_alerte_currentweek[,deb_sem:=as.character((as.Date(deb_sem,origin="1970-01-01")))]
  propsite_alerte_minsan[,deb_sem:=as.character((as.Date(deb_sem,origin="1970-01-01")))]
  propsite_alerte_minsan_byfacies[,deb_sem:=as.character((as.Date(deb_sem,origin="1970-01-01")))]
  #
  
  return (list(minsan_alerte_currentweek=minsan_alerte_currentweek,
               propsite_alerte_minsan=propsite_alerte_minsan,
               propsite_alerte_minsan_byfacies=propsite_alerte_minsan_byfacies,
               mydata=data
  ))
  

}