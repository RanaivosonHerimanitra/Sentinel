
  mydata= tdr_malaria(htc=F)
  semaine=array()
  max_deb_sem=max(as.Date(mydata$deb_sem,origin="1970-01-01"))
  semaine[6]= max_deb_sem-2*7 
  p=1
  for ( k in 2:6 )
  {
    semaine[p]=semaine[6]-(k-1)*7
    p=p+1
  }
  #select last 07 seven weeks for each disease:
  
  #Malaria selection for last 6 weeks before current week:
  Malaria=unique(mydata[as.Date(deb_sem,origin="1970-01-01") %in% semaine,list(deb_sem,sites,malaria_cases)],by=NULL)
  
  # reverse time order (to be conform to the report)
  setorder(Malaria,sites,deb_sem)
  
  #transform NA
  Malaria$malaria_cases=as.character(Malaria$malaria_cases)
  Malaria[is.na(malaria_cases)==T,malaria_cases:="na"]
  Malaria[,malaria_cases:=ifelse(nchar(malaria_cases)<2,paste0("0",malaria_cases),malaria_cases)]
  
  #get unique sites for max
  liste_sites= unique(Malaria$sites)
  for ( s in unique(Malaria$deb_sem) ) #foreach unique semaine
  { 
    #if not all sites in the current week in the total liste
    currentsiteweek= Malaria[as.Date(deb_sem,origin="1970-01-01") == as.Date(s,origin="1970-01-01"),get('sites')]
    if (  all (liste_sites %in% currentsiteweek   )==F )
    {
      missing_site=liste_sites[which(!(liste_sites %in%  currentsiteweek))]
      for ( p in missing_site)
      {
        tmp1 = data.table(deb_sem=as.Date(s,origin="1970-01-01"),sites=p,malaria_cases="na")
        var_conv(Malaria,tmp1)
        Malaria=rbind(Malaria,tmp1)
        rm(tmp1)
      }
    }
  }
  setorder(Malaria,deb_sem,sites)
  
  #keep unique obs.
  
  #inline historical occurence:
  Malaria[,vals:=paste(malaria_cases,collapse = "-"),by="sites"]
  #append latest week per site:
  Malaria[,malaria_cases:=NULL]
  
  lastweek = unique(mydata[as.Date(deb_sem,origin="1970-01-01")==max_deb_sem-7,list(sites,malaria_cases)],by=NULL)
  lastweek[,malaria_cases:=as.character(malaria_cases)]
  lastweek[is.na(malaria_cases)==T,malaria_cases:="na"]
  Malaria= merge(unique(Malaria[,list(sites,vals)],by=NULL),lastweek, 
                 by.x="sites", by.y="sites")
  #merge with sentinel_latlong to display name
  Malaria=merge(Malaria,sentinel_latlong[,list(sites,name)],
                by.x="sites",by.y="sites")
  #
  setnames(Malaria,c("name","vals","malaria_cases"),
           c("Sites","6 semaines précédentes","Semaine dernière"))
  
  ############################################################################
  
  #Fever selection for last 6 weeks before current week:
  fievre=mydata[as.Date(deb_sem,origin="1970-01-01") %in% semaine,list(deb_sem,sites,SyndF)]
  fievre=fievre[,SyndF:=sum(SyndF,na.rm = T),by="deb_sem,sites"]
  fievre=unique(fievre,by=NULL)
  #transform NA into XX
  fievre$SyndF=as.character(fievre$SyndF)
  fievre[is.na(SyndF)==T,SyndF:="na"]
  fievre[,SyndF:=ifelse(nchar(SyndF)<2,paste0("0",SyndF),SyndF)]
  #looking for missing sites for each semaine:
  
  #get unique sites for max
  liste_sites= unique(fievre$sites)
  for ( s in unique(fievre$deb_sem) ) #foreach unique semaine
  { 
    currentsiteweek= fievre[as.Date(deb_sem,origin="1970-01-01") == as.Date(s,origin="1970-01-01"),get('sites')]
    if (  all ( liste_sites %in% currentsiteweek   )==F )
    {
      missing_site=liste_sites[which(!(liste_sites %in%  currentsiteweek))]
      for ( p in missing_site)
      {
        #cat(p," is missing for semaine(fiever):",s,"\n")
        tmp1 = data.table(deb_sem=as.Date(s,origin="1970-01-01"),sites=p,SyndF="na")
        var_conv(fievre,tmp1)
        fievre=rbind(fievre,tmp1)
        rm(tmp1)
      }
    }
  }
  setorder(fievre,deb_sem,sites)
  #
  
  #inline historical occurence:
  fievre[,vals:=paste(SyndF,collapse = "-"),by="sites"]
  
  #append latest week per site:
  fievre[,SyndF:=NULL]
  lastweek= unique(mydata[as.Date(deb_sem,origin="1970-01-01")==max_deb_sem-7, list(sites,SyndF)],by=NULL)
  lastweek[,SyndF:=sum(SyndF),by="sites"]
  lastweek=unique(lastweek,by=NULL)
  
  lastweek[,SyndF:=as.character(SyndF)]
  lastweek[is.na(SyndF)==T,SyndF:="na"]                           
  fievre= merge(unique(fievre[,list(sites,vals)],by=NULL),
                lastweek , by.x="sites", by.y="sites")
  #merge with sentinel_latlong to display name
  fievre=merge(fievre,sentinel_latlong[,list(sites,name)],
               by.x="sites",by.y="sites")
  # set names:               
  setnames(fievre,c("name","vals","SyndF"),
           c("Sites","6 semaines précédentes","Semaine dernière"))
  #save data to be re-used later:
  save(fievre,Malaria,file="interactive_summary_report/last6_fever_malaria.rda")
  #finalize data.frame to be output ed:
  fievre=fievre[,c("Sites","6 semaines précédentes","Semaine dernière"),with=F]
  Malaria=Malaria[,c("Sites","6 semaines précédentes","Semaine dernière"),with=F]
  
  return( list(fievre=fievre,Malaria=Malaria))

