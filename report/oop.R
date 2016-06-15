require(R6)
Site <- R6Class("Site",
                  public = list(
                    report.name=NA,
                    site.name = NA,
                   # report.part=NA,
                    initialize = function(report.name) {
                      self$report.name= report.name
                      self$load_data()
                      self$preprocess_data()
                      self$generate_main_chart()
                    },
                    add_table = function() {
                     self$generate_table(report.name)
                   },
                    add_individual_chart = function() {
                     self$generate_individual_chart(report.name)
                   },
                    load_data = function() {
                      source("./import_data.R")
                      source("./tdrplus.R")
                      source("./var_conversion.R")
                      source("./reporting_plot.R",local = T)
                    },
                    preprocess_data = function() {
                      if (self$report.name=="Palu_sentinelles") {
                        # Malaria 
                        mydata= tdr_malaria(htc=F)
                        source("preprocessing.R")
                        mydata=preprocessing_disease(include_all_sites=F)
                        sentinel=fread("data/sentinel_codes.csv")
                        setnames(sentinel,c("Centre","Code"),c("name","sites") )
                        sentinel[,sites:=tolower(sites)]
                        sentinel_latlong=rbind(sentinel_latlong,
                                               sentinel[!(sites %in% sentinel_latlong$sites)],
                                               fill=T)
                        Malaria=mydata[["Malaria"]]
                        Malaria[,occurence:=sum(occurence,na.rm = T),by="code"]
                        Malaria=unique(Malaria[,list(deb_sem,code,occurence)],by=NULL)
                        # ILI 
                        ili=as.data.table(gather(ili,key=sites,value=Synd_g,-c(code,deb_sem)))
                        ili[,Synd_g:=sum(Synd_g,na.rm = T),by="code"]
                        ili=unique(ili[,list(deb_sem,code,Synd_g)],by=NULL)
                      }
                      
                    },
                    generate_main_chart = function() {
                      if (self$report.name =="Palu_sentinelles")
                      {
                        source("oop-report/Palu_sentinelles_main_chart.R",local = T)
                      }
                    },
                    generate_table = function() {
                      if (self$report.name=="Palu_sentinelles") {
                        #mydata= tdr_malaria(htc=F)
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
                        malaria=unique(mydata[as.Date(deb_sem,origin="1970-01-01") %in% semaine,list(deb_sem,sites,malaria_cases)],by=NULL)
                        
                        # reverse time order (to be conform to the report)
                        setorder(malaria,sites,deb_sem)
                        
                        #transform NA
                        malaria$malaria_cases=as.character(malaria$malaria_cases)
                        malaria[is.na(malaria_cases)==T,malaria_cases:="na"]
                        malaria[,malaria_cases:=ifelse(nchar(malaria_cases)<2,paste0("0",malaria_cases),malaria_cases)]
                        
                        #get unique sites for max
                        liste_sites= unique(malaria$sites)
                        for ( s in unique(malaria$deb_sem) ) #foreach unique semaine
                        { 
                          #if not all sites in the current week in the total liste
                          currentsiteweek= malaria[as.Date(deb_sem,origin="1970-01-01") == as.Date(s,origin="1970-01-01"),get('sites')]
                          if (  all (liste_sites %in% currentsiteweek   )==F )
                          {
                            missing_site=liste_sites[which(!(liste_sites %in%  currentsiteweek))]
                            for ( p in missing_site)
                            {
                              tmp1 = data.table(deb_sem=as.Date(s,origin="1970-01-01"),sites=p,malaria_cases="na")
                              var_conv(malaria,tmp1)
                              malaria=rbind(malaria,tmp1)
                              rm(tmp1)
                            }
                          }
                        }
                        setorder(malaria,deb_sem,sites)
                        
                        #keep unique obs.
                        
                        #inline historical occurence:
                        malaria[,vals:=paste(malaria_cases,collapse = "-"),by="sites"]
                        #append latest week per site:
                        malaria[,malaria_cases:=NULL]
                        
                        lastweek = unique(mydata[as.Date(deb_sem,origin="1970-01-01")==max_deb_sem-7,list(sites,malaria_cases)],by=NULL)
                        lastweek[,malaria_cases:=as.character(malaria_cases)]
                        lastweek[is.na(malaria_cases)==T,malaria_cases:="na"]
                        malaria= merge(unique(malaria[,list(sites,vals)],by=NULL),lastweek, 
                                       by.x="sites", by.y="sites")
                        #merge with sentinel_latlong to display name
                        malaria=merge(malaria,sentinel_latlong[,list(sites,name)],
                                      by.x="sites",by.y="sites")
                        #
                        setnames(malaria,c("name","vals","malaria_cases"),
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
                        save(fievre,malaria,file="interactive_summary_report/last6_fever_malaria.rda")
                        #generate tables:
                        knitr::kable(fievre[,c("Sites","6 semaines précédentes","Semaine dernière"),with=F],caption="Fièvres dans le réseau sentinelle")
                        knitr::kable(malaria[,c("Sites","6 semaines précédentes","Semaine dernière"),with=F],caption="Paludisme dans le réseau sentinelle")
                      }
                    },
                    generate_individual_chart = function(report.name) {
                      if (report.name=="Palu_sentinelles") {
                        generate_plot(htc="dsd",
                                      mydata=mydata,
                                      disease.name="malaria",
                                      disease1=malaria,
                                      disease2=fievre,
                                      disease1.targetvar="malaria_cases",
                                      disease2.targetvar="SyndF",
                                      legend.disease1="Palu(TDR+)",
                                      legend.disease2="Fièvres",
                                      title.label.list="Fièvres et Palu à",
                                      title.label="Fièvres et Paludisme dans le réseau sentinelle",
                                      title.ylab="Cas de Fièvres et Paludisme")
                      }
                   }
                   
                    
                  )
)
Person <- R6Class("Person",
                  public = list(
                    name = NA,
                    hair = NA,
                    initialize = function(name, hair) {
                      if (!missing(name)) self$name <- name
                      if (!missing(hair)) self$hair <- hair
                      x=self$greet()
                      print(x)
                      # if (x=="Hello, my name is Ann.") {
                      #   cat("You can access it!")
                      # }
                    },
                    set_hair = function(val) {
                      self$hair <- val
                    },
                    greet = function() {
                      cat(paste0("Hello, my name is ", self$name, ".\n"))
                    }
                  )
)