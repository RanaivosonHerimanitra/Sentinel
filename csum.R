#Need grain fined control of the user interface (todo list)
calculate_csum = function (data=mydata,
                           Csum_year_map=input$Csum_year_map,
                           Csum_week_map=input$Csum_week_map,
                           Sd_csum_map=input$Sd_csum_map,
                           week_choice=ifelse(Sys.Date()-as.Date(paste0(year(Sys.Date()),"-01-01"))<8
                                              ,1,week(Sys.Date())),
                           week_Csum_map=input$week_Csum_map,
                           year_choice=year(Sys.Date()),byvar="code")
{
  #take most recent week in the data:
  max_deb_sem= max(as.Date(data$deb_sem))
  
  cat('year range during which moving average will be calculated are: ')
  year_range= (as.numeric(year_choice)-Csum_year_map):(as.numeric(year_choice)-1)
  cat(year_range,"DONE\n")
  
    if ( (Csum_week_map %% 2)==0 ) #if Odd number
    {
      alignement="right"
      Lright= round((Csum_week_map - 1)/2)
      Lleft = Lright -1
    } else {
      alignement="center" 
      #No problem here because Csum_week_map will be at the center
      #define L (lag to right and left)
      Lright= (Csum_week_map - 1)/2
      Lleft= (Csum_week_map - 1)/2
    }
  
  #sites and weeks of the current year
  mysites= unique(data[years %in% year(Sys.Date()),get("sites")])
  mydebsem =unique(data[years %in% year(Sys.Date()) ,get("deb_sem")])
  
  #Csum algo begins:
  csum_alerte= data[years == year(Sys.Date()),]  #init
  csum_alerte[,alert_status:="normal"] #init
  for ( w in mydebsem )
  {
    cat('Semaine epidemiologic:',w,'\n')
    #Past date range to be compared to current date_range in order to detect alert
    past_date_range= ( as.Date(w) -1*365 - Lleft*7 ):( as.Date(w)- 1*365  + Lright*7 )
    #loop thru all weeks of this year
    for ( p in 2:length(year_range) )
    {
      #(Sys.Date()-7) because latest finished week:
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
    csum_alerte[,nbsite_alerte:=1.0]; csum_alerte[,nbsite_normal:=1.0]
    csum_alerte[,myradius:=1]
    csum_alerte[alert_status=="alert",nbsite_alerte:=sum(occurence,na.rm = T)*1.0,by="sites,code"]
    csum_alerte[alert_status=="normal",nbsite_normal:=sum(occurence,na.rm = T)*1.0,by="sites,code"]
    csum_alerte[alert_status=="normal",myradius:=5*(nbsite_alerte+1)/sqrt(nbsite_normal+1)]
    csum_alerte[alert_status=="alert",myradius:=sqrt(nbsite_alerte+1)/(nbsite_normal+1)]
    csum_alerte[alert_status %in% NA, myradius:=10*myradius]
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
  propsite_alerte_csum=merge(propsite_alerte_csum,data[,list(code,deb_sem)],
                             by.x=byvar,by.y=byvar)
  
  ##############################prop by facies#######################################
  cat("Weekly number of sites in alert using C-sum algorithm (by facies)...")
  Nbsite_beyond=csum_alerte[ is.na(occurence)==F & alert_status=="alert",
                            length(unique(sites)),by=c(byvar,"facies")]
  cat("DONE\n")
  setnames(Nbsite_beyond,"V1","eff_beyond")
  cat("Weekly number of sites that had given data (by facies)...")
  Nbsite_withdata=csum_alerte[is.na(occurence)==F,
                              length(unique(sites)),by=c(byvar,"facies")]
  setnames(Nbsite_withdata,"V1","eff_total")
  cat("DONE\n")
  cat("MERGE to give proportion of sites...")
  propsite_alerte_csum_byfacies=merge(x=Nbsite_withdata,
                                      y=Nbsite_beyond,
                                      by.x=c(byvar,"facies"),
                                      by.y=c(byvar,"facies"),all.x=T)
  cat("DONE\n")
  cat("calculate prop and change NA to zero...")
  propsite_alerte_csum_byfacies[,prop:=ifelse(is.na(eff_beyond/eff_total)==T,0.0,
                                     eff_beyond/eff_total)]
  cat("DONE\n")
  cat("merge with deb_sem to reorder time series...")
  propsite_alerte_csum_byfacies=merge(propsite_alerte_csum_byfacies,
                                      data[,list(code,facies,deb_sem)],
                             by.x=c(byvar,"facies"),by.y=c(byvar,"facies"))
  rm(Nbsite_withdata);rm(Nbsite_beyond);gc()
  cat("DONE\n")
  
  return(list(
    csum_alerte_currentweek=csum_alerte[as.Date(deb_sem)==max_deb_sem-7,],
    csum_alerte=csum_alerte,
    propsite_alerte_csum=unique(propsite_alerte_csum),
    propsite_alerte_csum_byfacies=unique(propsite_alerte_csum_byfacies )
    )) 
}