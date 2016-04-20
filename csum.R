
calculate_csum = function (data=mydata,
                           Csum_year_map=input$Csum_year_map,
                           #Csum_week_map=input$Csum_week_map,
                           Sd_csum_map=input$Sd_csum_map,
                           week_choice=ifelse(Sys.Date()-as.Date(paste0(year(Sys.Date()),"-01-01"))<8
                                              ,1,isoweek(Sys.Date())),
                           week_Csum_map=input$week_Csum_map,
                           year_choice=year(Sys.Date()),byvar="code")
{
  #take 52 most recent week in the data:
  #because they will be compared to historical values (week per week)
  max_deb_sem= max(as.Date(data$deb_sem,origin=as.Date("1970-01-01")))
  max_code=paste0(year(max_deb_sem),"_",isoweek(max_deb_sem))
  last52weeks= unique(as.Date(data$deb_sem))[order(unique(as.Date(data$deb_sem)),decreasing = T)[1:52]]
  
  cat('year range during which moving average will be calculated are: ')
  year_range= (as.numeric(year_choice)-Csum_year_map):(as.numeric(year_choice)-1)
  cat(year_range,"DONE\n")
  week_Csum_map=as.numeric(week_Csum_map)
    if ( (week_Csum_map %% 2)==0 ) #if Odd number
    {
      alignement="right"
      Lright= round((week_Csum_map - 1)/2)
      Lleft = Lright -1
    } else {
      alignement="center" 
      #define L (lag to right and left)
      Lright= (week_Csum_map - 1)/2
      Lleft= (week_Csum_map - 1)/2
    }
  
  #sites and weeks for the last 52 weeks:
  mysites= unique(data[ as.Date(deb_sem) %in% last52weeks,get("sites")])
  mydebsem =unique(data[ as.Date(deb_sem) %in% last52weeks ,get("deb_sem")])
  
  #Csum algo begins:
  csum_alerte= data[as.Date(deb_sem) %in% last52weeks,]  #init
  
  csum_alerte[,alert_status:="normal"] #init
  for ( w in mydebsem )
  {
    cat('Semaine epidemiologic:',w,'\n')
    #Past date range to be compared to current date_range in order to detect alert
    past_date_range= ( as.Date(w) -1*365 - Lleft*7 ):( as.Date(w)- 1*365  + Lright*7 )
    #loop thru all weeks of this year
    for ( p in 2:length(year_range) )
    {
      
      past_date_range= c(past_date_range,
                         ( as.Date(w) -p*365 - Lleft*7 ):( as.Date(w)- p*365  + Lright*7 ))
    }
    
    cat("re-extract year_range and week_range...")
    year_range= unique(year(as.Date(past_date_range)))
    week_range= unique(week(as.Date(past_date_range)))
    week_range=ifelse(week_range<10,paste0("0",week_range),week_range)
    code_range=unlist(lapply(year_range,function(i) paste0(i,"_",week_range)))
    cat("DONE\n")
    
    cat("Calculate historical smoothed mean per site and per year for code range:",code_range,"...")
    tmp= data[code %in% code_range,mean(occurence,na.rm = T),by="sites,years"]
    setnames(tmp,"V1","smoothed_mean")
    cat("DONE\n")
    cat('Calculate mean of smoothed mean per site...')
    tmp = tmp [,mean(smoothed_mean),by="sites"]
    setnames(tmp,"V1","smoothed_mean")
    cat('DONE\n')
   
    cat("merge smoothed mean per site with data of the current year...")
    csum_alerte=merge(csum_alerte,tmp,by.x="sites",by.y="sites")
    cat('DONE\n')
   
    cat("Compare historical smoothed mean with week:",w,"...")
    csum_alerte[deb_sem==w,alert_status:=ifelse(occurence>=as.numeric(Sd_csum_map) + smoothed_mean,
                                      "alert","normal")]
    csum_alerte[,smoothed_mean:=NULL]
    cat("DONE\n")
  }
   


    
    
    cat('calculate radius for per site for Csum algorithm for  the current week...')
    #fixons la taille du cercle à 15 pour les alertes
    #la taille des cercles en situation normale est proportionnelle
    # au nombre de cas mais ne dépasse pas 15.
    csum_alerte[alert_status=="alert",myradius:=15.0]
    csum_alerte[alert_status=="normal",sum_occurence_week:=sum(occurence,na.rm=T),by="code"]
    csum_alerte[alert_status=="normal", myradius:=15*occurence/sum_occurence_week,by="sites,code"]
    #set a minimum value if less than 2.5 in radius (for visibility purpose):
    csum_alerte[alert_status=="normal", myradius:=ifelse(myradius<2.5,2.5,myradius),by="sites,code"]
    
    csum_alerte[alert_status %in% NA | myradius %in% NA , myradius:=5.0]
    
   
    cat('DONE\n')
    
  
  #########################################Weekly Proportion of sites in alert ###########
  cat("Weekly number of sites in alert using C-sum algorithm (all)...\n")
  Nbsite_beyond=csum_alerte[ is.na(occurence)==F & alert_status=="alert",length(unique(sites)),by=byvar]
  setnames(Nbsite_beyond,"V1","eff_beyond")
  cat('dimension of Nbsite_beyond for csum:',dim(Nbsite_beyond),'\n')
  cat("Weekly number of sites that had given data (all)...\n")
  Nbsite_withdata=csum_alerte[ is.na(occurence)==F,length(unique(sites)),by=byvar]
  setnames(Nbsite_withdata,"V1","eff_total")
  #MERGE to give proportion of sites beyond n_percentile per week:
  propsite_alerte_csum=merge(x=Nbsite_withdata,y=Nbsite_beyond,by.x=byvar,by.y=byvar,all.x=T)
  #calculate prop and change NA to zero:
  propsite_alerte_csum[,prop:=ifelse(is.na(eff_beyond/eff_total)==T,0.0,eff_beyond/eff_total)]
  
  
  #merge with deb_sem to reorder time series:
   propsite_alerte_csum=merge(propsite_alerte_csum,
                                csum_alerte[,list(code,deb_sem,sites,alert_status,East,
                                           South,High_land,Fringe,excepted_East,
                                           excepted_High_land)],
                                by.x="code",by.y="code")
  
   ####################new method to handle facies###################################
   list_facies= c("East","South","High_land","Fringe","excepted_East","excepted_High_land")
   datalist_facies=list()
   for ( f in list_facies)
   {
     cat("Weekly number of sites in alert using C-sum for ",f,"...")
      Nbsite_beyond=csum_alerte[ is.na(occurence)==F & alert_status=="alert" & get(f)==1,
                              length(unique(sites)),by=c(byvar,f)]
     cat("DONE\n")
      setnames(Nbsite_beyond,"V1","eff_beyond")
     cat("Weekly number of sites that had given data (by facies)...")
      Nbsite_withdata=csum_alerte[is.na(occurence)==F & get(f)==1,
                               length(unique(sites)),by=c(byvar,f)]
      setnames(Nbsite_withdata,"V1","eff_total")
     cat("DONE\n")
     cat("MERGE to give proportion of sites...")
     myfacies=merge(x=Nbsite_withdata,y=Nbsite_beyond,
                                      by.x=c(byvar,f),
                                      by.y=c(byvar,f),all.x=T)
     cat("DONE\n")
     cat("calculate prop and change NA to zero...")
     myfacies[,prop:=ifelse(is.na(eff_beyond/eff_total)==T,0.0,
                                               eff_beyond/eff_total)]
     cat("DONE\n")
     cat("merge with deb_sem to reorder time series...")
     myfacies=merge(myfacies,data[,list(code,deb_sem)],
                            by.x=c(byvar),by.y=c(byvar))
      rm(Nbsite_withdata);rm(Nbsite_beyond);gc()
     cat("DONE\n")
     datalist_facies[[f]]=myfacies
     #append to a single and unique dataframe:
     if ( f==list_facies[1])
     {
       propsite_alerte_csum_byfacies= datalist_facies[[f]]
       propsite_alerte_csum_byfacies[,f:=NULL,with=F]
       propsite_alerte_csum_byfacies[,eff_total:=NULL]
       propsite_alerte_csum_byfacies[,eff_beyond:=NULL]
       propsite_alerte_csum_byfacies[,facies:=f]
     } else {
       tmp= datalist_facies[[f]]
       tmp[,f:=NULL,with=F]
       tmp[,eff_total:=NULL]
       tmp[,eff_beyond:=NULL]
       tmp[,facies:=f]
       propsite_alerte_csum_byfacies=rbind(propsite_alerte_csum_byfacies,tmp)
       rm(tmp);gc()
     }
     cat("DONE\n")
     
   }
   
  
  
  if (max_code==paste0(year(Sys.Date()),"_",isoweek(Sys.Date())) ) {
    #if max_date == current week then exclude this current week
    #from calculation of alert , otherwise include 
    mycode=paste0(year(Sys.Date()-7),"_",isoweek(Sys.Date()-7))
    csum_alerte_currentweek=csum_alerte[code==mycode,]
  } else {
    csum_alerte_currentweek=csum_alerte[code==max_code,]
  }
  
   
   #convert to usual date format:
   csum_alerte_currentweek[,deb_sem:=as.character((as.Date(deb_sem,origin="1970-01-01")))]
   csum_alerte[,deb_sem:=as.character((as.Date(deb_sem,origin="1970-01-01")))]
   propsite_alerte_csum[,deb_sem:=as.character((as.Date(deb_sem,origin="1970-01-01")))]
   propsite_alerte_csum_byfacies[,deb_sem:=as.character((as.Date(deb_sem,origin="1970-01-01")))]
   #
  
  
  return(list(
    csum_alerte_currentweek=csum_alerte_currentweek,
    csum_alerte=csum_alerte,
    propsite_alerte_csum=propsite_alerte_csum,
    propsite_alerte_csum_byfacies=propsite_alerte_csum_byfacies 
    )) 
}