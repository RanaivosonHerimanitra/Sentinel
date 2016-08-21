require(R6)
Site <- R6Class("Site",
                  public = list(
                    report.name=NA,
                    initialize = function(report.name) {
                      #the moment you change the report:
                      self$report.name= report.name
                      #load data
                      self$load_data()
                      #preprocess data:
                      data_processed=self$preprocess_data()
                      if ( self$report.name=="Palu_sentinelles")
                      {
                        #retrieve result
                        Malaria=data_processed$value$Malaria
                        ili=data_processed$value$ili
                        #generate main chart
                        self$generate_main_chart(Malaria=Malaria,ili=ili,mydata=NULL)
                      }
                      if (self$report.name=="Diar_sentinelles") 
                      {
                        #retrieve result
                        diarrhea=data_processed$value$diarrhea
                        self$generate_main_chart(Malaria=NULL,ili=NULL,mydata=diarrhea)
                      }
                     
                      
                      
                    },
                    add_table = function() {
                      if ( self$report.name=="Palu_sentinelles" ) {
                        mytables=self$generate_table(Malaria=Malaria,ili=ili)
                        return(mytables)
                      }
                   },
                   #  add_individual_chart = function() {
                   #   self$generate_individual_chart()
                   # },
                    load_data = function() {
                      source("./import_data.R")
                      source("./algorithms/tdrplus.R")
                      source("./utils/var_conversion.R")
                      source("./reporting_plot.R",local = T)
                      source("./preprocessing.R")
                    },
                    preprocess_data = function() {
                      if (self$report.name=="Palu_sentinelles") {
                        source("oop-report/preprocess_data/Palu_sentinelles_preprocessing.R",local = T)
                      }
                      if (self$report.name=="Diar_sentinelles") {
                        source("oop-report/preprocess_data/Diar_sentinelles_preprocessing.R",local = T)
                      }
                      
                    },
                    generate_main_chart = function(Malaria,ili,mydata) {
                      if (self$report.name =="Palu_sentinelles")
                      {
                        source("oop-report/generate_main_chart/Palu_sentinelles_main_chart.R",local = T)
                      }
                      if (self$report.name =="Diar_sentinelles")
                      {
                        source("oop-report/generate_main_chart/Diar_sentinelles_main_chart.R",local = T)
                      }
                    },
                    generate_table = function(Malaria,ili) {
                      if (self$report.name=="Palu_sentinelles") 
                      {
                        source("oop-report/generate_table/Palu_sentinelles_generate_table.R",local = T)
                      }
                      if (self$report.name=="Diar_sentinelles") 
                      {
                        source("oop-report/generate_table/Diar_sentinelles_generate_table.R",local = T)
                      }
                    }
                   # ,
                   #  generate_individual_chart = function() {
                   #    if (self$report.name=="Palu_sentinelles") {
                   #      generate_plot(htc="dsd",
                   #                    mydata=mydata,
                   #                    disease.name="malaria",
                   #                    disease1=malaria,
                   #                    disease2=fievre,
                   #                    disease1.targetvar="malaria_cases",
                   #                    disease2.targetvar="SyndF",
                   #                    legend.disease1="Palu(TDR+)",
                   #                    legend.disease2="Fièvres",
                   #                    title.label.list="Fièvres et Palu à",
                   #                    title.label="Fièvres et Paludisme dans le réseau sentinelle",
                   #                    title.ylab="Cas de Fièvres et Paludisme")
                   #    }
                   # }
                  )
                )
