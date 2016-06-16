require(R6)
Site <- R6Class("Site",
                  public = list(
                    report.name=NA,
                    initialize = function(report.name) {
                      self$report.name= report.name
                      #load data
                      self$load_data()
                      #preprocess data:
                      data_processed=self$preprocess_data()
                      #retrieve result
                      Malaria=data_processed$Malaria
                      ili=data_processed$ili
                      #generate main chart
                      self$generate_main_chart(Malaria=Malaria,ili=ili)
                    },
                    add_table = function() {
                     mytables=self$generate_table(Malaria=Malaria,ili=ili)
                     return(mytables)
                   },
                    add_individual_chart = function() {
                     self$generate_individual_chart()
                   },
                    load_data = function() {
                      source("./import_data.R")
                      source("./algorithms/tdrplus.R")
                      source("./utils/var_conversion.R")
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
                        #return result:
                        return(list(Malaria=Malaria,ili=ili))
                      }
                      
                    },
                    generate_main_chart = function(Malaria,ili) {
                      if (self$report.name =="Palu_sentinelles")
                      {
                        source("oop-report/Palu_sentinelles_main_chart.R",local = T)
                      }
                    },
                    generate_table = function(Malaria,ili) {
                      if (self$report.name=="Palu_sentinelles") 
                      {
                        source("oop-report/Palu_sentinelles_generate_table.R",local = T)
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