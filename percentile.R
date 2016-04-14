calculate_percentile=function(data=mydata,
                              week_length=input$comet_map,
                              percentile_value=input$Centile_map
                              )
{

   max_deb_sem= max(as.Date(data$deb_sem))
   max_code=paste0(year(max_deb_sem),"_",isoweek(max_deb_sem))
   
    if (as.numeric(week_length)==4 )
    {
      week1 = Sys.Date()-1*7
      week2 = Sys.Date()-2*7
      week3 = Sys.Date()-3*7
      week4 = Sys.Date()-4*7
      week_range = c(week1,week2,week3,week4)
    }
    if (as.numeric(week_length)==3 )
    {
      week1 = Sys.Date()-1*7
      week2 = Sys.Date()-2*7
      week3 = Sys.Date()-3*7
      week_range = c(week1,week2,week3)
      
    }
    if (as.numeric(week_length)==2 )
    {
      week1 = Sys.Date()-1*7
      week2 = Sys.Date()-2*7
      week_range =c(week1,week2)
    }
   if (as.numeric(week_length)==1)
   {
     week1 = Sys.Date()-1*7
     week_range = week1
   }
    cat('retrieve epidemiological weeks corresponding to parameters...')
      code_range = unique(unlist(lapply(week_range,function(i) paste0( year(i),"_",ifelse(week(i)<10,paste0("0",week(i)),week(i)) ) )))[1:week_length]
    cat('DONE\n')
    cat("code range defined by user encompasses:",code_range,"\n")
    
    cat('calculate ',percentile_value,'-th percentile for all the data except for the current week...')
    setkey(data,deb_sem)
    if (max_code==paste0(year(Sys.Date()),"_",isoweek(Sys.Date())) ) {
      #if max_date == current week then exclude this current week
      #from calculation of alert , otherwise include 
      mypercentile=data[as.Date(deb_sem)<max_deb_sem,
                        quantile(occurence,probs=percentile_value/100,na.rm=T),by="sites"]
    } else {
      mypercentile=data[,quantile(occurence,probs=percentile_value/100,na.rm=T),by="sites"]
    } 
    setnames(mypercentile,old="V1",new="n_percentile")
    cat("DONE\n")
   
    
    cat("merge",percentile_value,"-percentile with all the data(selected disease)...")
    setkey(data,sites);setkey(mypercentile,sites)
    data=merge(data,mypercentile,by.x="sites",by.y="sites")
    cat("DONE\n")
    
    cat("initialize alert by cleaning NA's values...","\n")
    data[,occurence_cleaned:=ifelse(is.na(occurence)==T,0,occurence)]
    cat("detection of alerts for all weeks during which selected disease cases exceed",percentile_value,"%...")
    data[, alert:=ifelse(occurence_cleaned>n_percentile,1,0)]   
    cat("DONE\n")
    
    #new algo for detecting alert:
    data[,deb_sem:=as.Date(deb_sem)]
    cat("reorder date of the week by decreasing order, by sites...")
    setorder(data,sites,deb_sem)
    cat("DONE\n")
    
    #NEW ALGORITHM TO OUTPUT ALERT:
    lg = function (x) c( NA, x[-length(x)])
      data[,mylag1:=lg(occurence),by="sites"]
      data[,mylag2:=lg(mylag1),by="sites"]
      data[,mylag3:=lg(mylag2),by="sites"]
      
      data[,alert_status4:=ifelse(occurence>=n_percentile & mylag1>=n_percentile & mylag2>=n_percentile & mylag3>=n_percentile, "alert","normal")]
      data[,alert_status3:=ifelse(occurence>=n_percentile & mylag1>=n_percentile & mylag2>=n_percentile,"alert","normal")]
      data[,alert_status2:=ifelse(occurence>=n_percentile &  mylag1>=n_percentile ,"alert","normal")]
      data[,alert_status1:=ifelse(occurence>=n_percentile,"alert","normal")]
   
    #chosen week length to be renamed:
    setnames(data,paste0("alert_status",week_length),"alert_status")
    #END OF NEW ALGORITHM
    cat('recode alert_status into NA for those without data...')
    data[is.na(occurence)==T,alert_status:=NA]
    cat('DONE\n')

    cat('calculate radius per site for percentile algorithm alert...')
    data[,nbsite_alerte:=1.0]; data[,nbsite_normal:=1.0];
    data[,myradius:=1.0]
    
    ##################### new algo to determine radius of circle 
    # if no data then set radius to 5
    # if alert then set radius to 15
    # if not normal then  15*(weighted sum of # cases)
    # l'idée c'est de différencier les sites selon nombre de cas selon les sites mais de se
    #fixer un max size de 15
    data[alert_status=="alert", myradius:=15.0]
    data[alert_status=="normal",sum_occurence_week:=sum(occurence,na.rm=T),by="code"]
    data[alert_status=="normal", myradius:=15.0*occurence/sum_occurence_week,by="sites,code"]
    #set a minimum value if less than 2.5 in radius (for visibility purpose):
    data[alert_status=="normal" & is.na(myradius)==F, myradius:=ifelse(myradius<2.5,2.5,myradius),by="sites,code"]
    
    data[alert_status %in% NA | myradius %in% NA , myradius:=5.0]
    
    
    
    cat('DONE\n')
  

    
    cat("selected sites for:",code_range,"with alert status\n")
    percentile_alerte=data[code %in% code_range,list(sites,code,alert_status,deb_sem,myradius)]
    cat('DONE\n')
    
    #cat("prepare alert to be displayed on the map (latest finished week)...")
   
    
    if (max_code==paste0(year(Sys.Date()),"_",isoweek(Sys.Date())) ) {
      #if max_date == current week then exclude this current week
      #from calculation of alert , otherwise include 
      mycode=paste0(year(max_deb_sem-7),"_",isoweek(max_deb_sem-7))
     
      percentile_alerte_currentweek=percentile_alerte[code==mycode,]
    } else {
      
      percentile_alerte_currentweek=percentile_alerte[code==max_code,]
    }
    cat("DONE\n")
    
  cat("calculate weekly prop of sites in alert (all)...")
   Nbsite_beyond=data[is.na(occurence)==F & alert_status=="alert",length(unique(sites)),by="code"]
   setnames(Nbsite_beyond,"V1","eff_beyond")
   Nbsite_withdata=data[is.na(occurence)==F,length(unique(sites)),by="code"]
   setnames(Nbsite_withdata,"V1","eff_total")
   propsite_alerte_percentile=merge(x=Nbsite_withdata,
                                    y=Nbsite_beyond,
                                    by.x="code",by.y="code",all.x=T)
   propsite_alerte_percentile[,prop:=ifelse(is.na(eff_beyond/eff_total)==T,0.0,eff_beyond/eff_total)]
  cat("DONE\n")
  
 
  
  cat("merge with deb_sem and sites to reorder time series (all)...\n")
   propsite_alerte_percentile=merge(propsite_alerte_percentile,
                                   data[,list(code,deb_sem,sites,alert_status,East,
                                              South,High_land,Fringe,excepted_East,
                                              excepted_High_land)],
                                   by.x="code",by.y="code")
  
   rm(Nbsite_withdata);rm(Nbsite_beyond);gc()
  cat('DONE\n')

#################new method to handle facies ###################################  
  list_facies= c("East","South","High_land","Fringe","excepted_East","excepted_High_land")
  datalist_facies=list()
  for ( f in list_facies)
  {
    cat("calculate weekly prop of sites in alert using percentile in ",f,"...")
    Nbsite_beyond=data[is.na(occurence)==F & alert_status=="alert" & get(f)==1,length(unique(sites)),by=c("code",f)]
    setnames(Nbsite_beyond,"V1","eff_beyond")
    Nbsite_withdata=data[is.na(occurence)==F & get(f)==1,length(unique(sites)),by=c("code",f)]
    setnames(Nbsite_withdata,"V1","eff_total")
    myfacies=merge(x=Nbsite_withdata,
                   y=Nbsite_beyond,
                   by.x=c("code",f),
                   by.y=c("code",f),all.x=T)
    myfacies[,prop:=ifelse(is.na(eff_beyond/eff_total)==T,0.0,eff_beyond/eff_total)]
    datalist_facies[[f]]=myfacies

    #append to a single and unique dataframe:
    if ( f==list_facies[1])
    {
      propsite_alerte_percentile_byfacies= datalist_facies[[f]]
      # merge with deb_sem:
      propsite_alerte_percentile_byfacies=merge(propsite_alerte_percentile_byfacies,
                                                data[get(f)==1,list(code,deb_sem)],
                                                by.x="code",by.y="code")
      propsite_alerte_percentile_byfacies[,f:=NULL,with=F]
      propsite_alerte_percentile_byfacies[,eff_total:=NULL]
      propsite_alerte_percentile_byfacies[,eff_beyond:=NULL]
      propsite_alerte_percentile_byfacies[,facies:=f]
    } else {
      tmp= datalist_facies[[f]]
      # merge with deb_sem:
      tmp=merge(tmp,data[get(f)==1,list(code,deb_sem)],by.x="code",by.y="code")
      tmp[,f:=NULL,with=F]
      tmp[,eff_total:=NULL]
      tmp[,eff_beyond:=NULL]
      tmp[,facies:=f]
      propsite_alerte_percentile_byfacies=rbind(propsite_alerte_percentile_byfacies,tmp)
      rm(tmp);gc()
    }
    cat("DONE\n")
  }
 
###############################################################################
  
  
  
  # cat("calculate weekly prop of sites in alert using percentile algorithm (by faciès)...")
  #  Nbsite_beyond=data[is.na(occurence)==F & alert_status=="alert",length(unique(sites)),by=c("code","facies")]
  #  setnames(Nbsite_beyond,"V1","eff_beyond")
  #  Nbsite_withdata=data[is.na(occurence)==F,length(unique(sites)),by=c("code","facies")]
  #  setnames(Nbsite_withdata,"V1","eff_total")
  #  propsite_alerte_percentile_byfacies=merge(x=Nbsite_withdata,
  #                                            y=Nbsite_beyond,
  #                                            by.x=c("code","facies"),
  #                                            by.y=c("code","facies"),all.x=T)
  #  propsite_alerte_percentile_byfacies[,prop:=ifelse(is.na(eff_beyond/eff_total)==T,0.0,eff_beyond/eff_total)]
  # cat("DONE\n")
  
  
  ### TODO==>MUST CHECK WHY ONLY 02 FACIES FOR data?????
  ### because create_facies.R is not valid!!!!
  
  # cat("merge with deb_sem and sites to reorder time series (by faciès)...")
  #  setkeyv(propsite_alerte_percentile_byfacies,c("code","facies"))
  #  setkeyv(data,c("code","facies"))
  #  propsite_alerte_percentile_byfacies=merge(propsite_alerte_percentile_byfacies,
  #                                  unique(data[,list(code,deb_sem,facies)]),
  #                                  by.x=c("code","facies"),by.y=c("code","facies"))
  #  rm(Nbsite_withdata);rm(Nbsite_beyond);gc()
  # cat('DONE\n')
  
  print(percentile_alerte_currentweek[sites=="ejd"]); 
  
  return (list(percentile_alerte=percentile_alerte,
               percentile_alerte_currentweek=percentile_alerte_currentweek,
               propsite_alerte_percentile=propsite_alerte_percentile,
               propsite_alerte_percentile_byfacies=propsite_alerte_percentile_byfacies,
               mydata=data
               ))
}
