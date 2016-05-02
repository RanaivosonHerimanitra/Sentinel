
myradio2= radioButtons(inputId="Algorithmes_viz", label="Algorithms:",
                          list(  "Percentile" = "Percentile",
                                 "MinSan" = "MinSan",
                                 "C-SUM" = "Csum",
                                 "RDT+/fever Indicator" = "Ind"))


source("heatmap_ui.R",local = T)

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

#plot.new()
source("params_ui.R",local = T)
choose_disease_report= box(title="Reporting",
                           status = "primary", 
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           tags$p("This report includes historical alerts about Malaria and diarrhea. Algorithm used is 90th percentile.
                                   It also contains information about lack of data and RDT compared to reported number of fever's consultation diagnosis."),
                           downloadButton('downloadReport','Download',class="primary")
                           ,width=8)
                                   
source("map_ui.R",local = T)
algoviz_parameters=box(status = "primary", solidHeader = TRUE,
                       collapsible = TRUE,title="Parameters")

source("viz_ui.R",local = T)
#UI for the bubble chart:
source("pastalert_ui.R",local = T)
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
source("forecast_ui.R")
forecast_item=tabItem(tabName="myforecast",myforecast_ui)
disease_item=tabItem(tabName = "diparam",
                     fluidRow(choose_disease_report)  )
