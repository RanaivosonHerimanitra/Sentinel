
myradio2= radioButtons(inputId="Algorithmes_viz", label="Algorithms:",
                          list(  "Percentile" = "Percentile",
                                 "MinSan" = "MinSan",
                                 "C-SUM" = "Csum",
                                 "RDT+/fever Indicator" = "Ind"))


source("UI/heatmap_ui.R",local = T)

####################################Visualization############################
graph_choices= selectInput(inputId="Algorithmes_viz", label="Choose algorithms:",
                                choices=c("Malaria cases",
                                          "MinSan",
                                          "Percentile",
                                          "C-SUM",
                                          "RDT+/fever Indicator"),
                           selected = "Percentile"
                   )
#map parameters:
source("handle_dates.R")
year_retrospective=selectInput("year_choice", label="Select a year",
                               year(Sys.Date()):(year(Sys.Date())-1),
                               selected= year(Sys.Date()))
week_retrospective=sliderInput(inputId="week_choice", 
            "Choose a Week and year for which to display alert:", 
            min = 1,
            max = ifelse(Sys.Date()-as.Date(paste0(year(Sys.Date()),"-01-01"))<8
                         ,1,week(Sys.Date())), 
            step=4, #per month
            value = ifelse(Sys.Date()-as.Date(paste0(year(Sys.Date()),"-01-01"))<8
                           ,1,week(Sys.Date())) )


source("UI/params_ui.R",local = T)

                                   
source("UI/map_ui.R",local = T)
algoviz_parameters=box(status = "primary", solidHeader = TRUE,
                       collapsible = TRUE,title="Parameters")

source("UI/viz_ui.R",local = T)
#UI for the bubble chart:
source("UI/pastalert_ui.R",local = T)
#
tabbox_item= tabItem(tabName = "mytabbox",
                     fluidRow(
                       map_parameters,
                       box(
                       tabBox(
                         width = 12,
                         id = "tabset1", 
                         tabPanel("Map", map_display ),
                         tabPanel("Diseases calendar", heatmap_display),
                         tabPanel("Visualize", algoviz_display),
                         tabPanel(list("Explore past alerts",  
                                         tags$small(class="media-heading",
                                                    tags$span(class="label label-danger", "beta"))),
                                  pastalert_display)
                       ),width = 9)
                       
                     ))
##############################layer of the forecast page:#################
source("UI/forecast_ui.R")
forecast_item=tabItem(tabName="myforecast",myforecast_ui)

#######################layer of the summary report and download page:####
choose_disease_report= box(title="Reporting",
                           status = "primary", 
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           tags$p("This report includes historical alerts about Malaria, diarrhea and PFA across 54 sentinel sites. Algorithm used is 90th percentile.
                                   It also contains information about lack of data and RDT+ compared to reported number of fever's consultation diagnosis."),
                           downloadButton('downloadReport','Download',class="primary")
                           ,width=12)
source("interactive_summary_report/layers.R")
disease_item=tabItem(tabName = "diparam",
                     fluidRow(tabBox(width = 12,
                                     tabPanel("Summary report",summary_report),
                                     tabPanel("HTC report",htc_report),
                                     tabPanel("Malaria-Fever report",malaria_report),
                                     tabPanel("Diarrhea report",diarrhea_report),
                                     tabPanel("ILI report",ili_report),
                                     tabPanel("Acute Flaccid Paralysis (AFP) report",pfa_report),
                                     tabPanel("Missing sent report",missing_sent_report))),
                     fluidRow(choose_disease_report)
                     )
