
myradio2= radioButtons(inputId="Algorithmes_viz", label="Algorithms:",
                          list(  "Percentile" = "Percentile",
                                 "MinSan" = "MinSan",
                                 "C-SUM" = "Csum",
                                 "RDT+/fever Indicator" = "Ind"))


source("heatmap_ui.R",local = T)

mycondparam_cumsum=conditionalPanel(
  condition = "input.Algorithmes == 'Csum'",
  tags$hr(),
  tags$p(tags$strong("Please select data aggregation level (by facies)")),
  selectInput("Cluster_csum", "",
              list("National" = "Total","East"="East","South"="South", "Central High Land"="High_land","Fringe"="Fringe","National except East"="excepted_East","National except Central High Land"="excepted_High_land")),
  tags$hr(),
  sliderInput("Csum_year", 
              "Years number:", 
              min = 1,
              max = 6, 
              value = 5
  ),
  tags$hr(),
  sliderInput("Csum_week", "Weeks number:", 
              min = 1,
              max = 26, 
              value = 3),
  tags$hr(),
  sliderInput("Sd_csum", "standard deviation:", 
              min = 1,
              max = 10,
              step=0.01,
              value = 2),
  tags$hr(),
  tags$p("This option is intended to improve the specificity of the alert system"),
  radioButtons("week_Csum", "Number of consecutive weeks above threshold:",
               c("1"="0","2"="1","3" = "2")),
  br(),
  helpText("The cumulative sum (C-SUM) method for epidemic detection is based on the construction of an average or base year by calculating the expected number of cases using the average for that week (and the previous and following week) during the past 5 years.")
)

mycondparam_ind=conditionalPanel(
  condition = "input.Algorithmes == 'Ind'",
  tags$hr(),
  tags$p(("Please select data aggregation level (by facies)")),
  selectInput("Cluster_ind", "",
              list("National" = "Total","East"="East","South"="South", "Central High Land"="High_land","Fringe"="Fringe","excepted East"="excepted_East","excepted Central High Land"="excepted_High_land")),
  sliderInput("exp", 
              "Malaria cases among fever cases:", 
              min = 0,
              max = 100, 
              value = 40
  ),
  sliderInput("expC", 
              "malaria cases among total consultations:", 
              min = 0,
              max = 100, 
              value = 10
  ),
  br(),
  sliderInput("daterange_ind", 
              "Number of years to display:", 
              min = 1,
              max = 7, 
              value = 6),
  br(),
  helpText("Malaria cases among fever cases. Indicator is considered, as high when ratio is greater than 40%, and malaria cases among consultants number is greater than 10%.")     
)
####################################Visualization############################
graph_choices= selectInput(inputId="Algorithmes_viz", label="Choose algorithms:",
                                choices=c("Malaria cases",
                                          "MinSan",
                                          "Percentile",
                                          "C-SUM",
                                          "RDT+/fever Indicator"),
                           selected = "Percentile"
                   )

Mcases_graph=conditionalPanel(
  #condition = "input.Algorithmes == 'Mcases'",
  h6('Malaria cases since 01-01-2015'),
  showOutput("malariacases", "highcharts"),
  helpText("Click on the legend to hide/show variables"),
  helpText("Click and drag on the graph to zoom"),
  tags$p(tags$strong("Legend")),
  helpText("RDT+, Number of reported Malaria cases by epidemiological week"), 
  helpText("Weekly mean (2009-2015)"), 
  helpText("90th percentile (threshold)"))

Csum_graph=conditionalPanel(
  #condition = "input.Algorithmes == 'Csum'",
  h4('Cumulative Sum'),
  showOutput("myCsum", "highcharts"),
  helpText("Click on the legend to hide/show variables"),
  helpText("Click and drag on the graph to zoom"),
  tags$p(tags$strong("Legend")),
  helpText("Alert, is proportion of site in alert across selected facies in the side panel."), 
  helpText("Rainfall, African Rainfall Estimation (RFE) is produced by NOAA-CPC"), 
  helpText("NDVI, is a normalized difference vegetation index (NDVI)  produced by MODIS"), 
  helpText("Temperature, Land Surface temperature is an estimation of near surface temperature, produced by MODIS"),
  helpText("IRS, proportion of sites that received a IRS "),
  helpText("LLIN, proportion of sites that received a LLIN "))

MoySd_graph=conditionalPanel(
  #condition = "input.Algorithmes == 'MoySd'",
  h4('Mean & Standard deviation'),
  showOutput("myMoy", "highcharts"),
  helpText("Click on the legend to hide/show variables"),
  helpText("Click and drag on the graph to zoom"),
  tags$p(tags$strong("Legend")),
  helpText("Alert, is proportion of site in alert across selected facies in the side panel."), 
  helpText("Rainfall, African Rainfall Estimation (RFE) is produced by NOAA-CPC"), 
  helpText("NDVI, is a normalized difference vegetation index (NDVI)  produced by MODIS"), 
  helpText("Temperature, Land Surface temperature is an estimation of near surface temperature, produced by MODIS"),
  helpText("IRS, proportion of sites that received a IRS "),
  helpText("LLIN, proportion of sites that received a LLIN "))

Ind_graph=conditionalPanel(
  #condition = "input.Algorithmes == 'Ind'",
  h4('Fever indicator'),
  showOutput("myInd", "highcharts"),
  helpText("Click on the legend to hide/show variables"),
  helpText("Click and drag on the graph to zoom"),
  tags$p(tags$strong("Legend")),
  helpText("Alert, is proportion of site in alert across selected facies in the side panel."), 
  helpText("Rainfall, African Rainfall Estimation (RFE) is produced by NOAA-CPC"), 
  helpText("NDVI, is a normalized difference vegetation index (NDVI)  produced by MODIS"), 
  helpText("Temperature, Land Surface temperature is an estimation of near surface temperature, produced by MODIS"),
  helpText("IRS, proportion of sites that received a IRS "),
  helpText("LLIN, proportion of sites that received a LLIN "))

MinSan_graph=conditionalPanel(
  #condition = "input.Algorithmes == 'MinSan'",
  h4('Ministry of Health'),
  showOutput("myMinSan", "highcharts"),
  helpText("Click on the legend to hide/show variables"),
  helpText("Click and drag on the graph to zoom"),
  tags$p(tags$strong("Legend")),
  helpText("Alert, is proportion of site in alert across selected facies in the side panel."), 
  helpText("Rainfall, African Rainfall Estimation (RFE) is produced by NOAA-CPC"), 
  helpText("NDVI, is a normalized difference vegetation index (NDVI)  produced by MODIS"), 
  helpText("Temperature, Land Surface temperature is an estimation of near surface temperature, produced by MODIS"),
  helpText("IRS, proportion of sites that received a IRS "),
  helpText("LLIN, proportion of sites that received a LLIN "))

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
                           It also contains information about lack of data and RDT compared to reported number of fiever's consultation."),
                    
                    downloadButton('downloadReport',
                                   'Download',class="primary"),width=8)
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
