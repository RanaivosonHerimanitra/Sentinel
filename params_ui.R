############################Main parameters UI ############################
sentinel_sites=selectInput(inputId="mysites", 
                           label="Choose a site:", 
                           choices = c("Farafangana","Ambovombe",
                                       "Ambatondrazaka","Antsohihy",
                                       "Anjozorobe","Antsirabe",
                                       "Belo sur Tsiribihina","Behoririka",
                                       "Ambato Boeny","Ambositra",
                                       "Cd Andohatapenaka","Diego",
                                       "Mandritsara","Edjeda","Fianarantsoa",
                                       "Ihosy","Maevatanana","Morondava",
                                       "Mahajanga","Miandrivazo",
                                       "Manjakaray","Mananjary",
                                       "Morombe","Moramanga",
                                       "Maroantsetra","Maintirano",
                                       "Nosy Be","Sambava","Sainte-Marie",
                                       "Tsiroanomandidy","Tolagnaro","Toliary",
                                       "Toamasina","Tsaralalana"),
                           selected="Farafangana")
                           
myfacies_algo=selectInput(inputId ="Cluster_algo", 
                          label="Data aggregation level (facies)",
                          list("National" = "Total",
                               "East"="East",
                               "South"="South", 
                               "Central High Land"="High_land",
                               "Fringe"="Fringe",
                               "National except East"="excepted_East",
                               "National except Central High Land"="excepted_High_land"))

diseases_choices=selectInput(inputId = "diseases", label="Select a disease to monitor",
                             choices=list("Malaria" = "Malaria",
                                          "Diarrhée"="Diarrhée"
                             ),selected = "Malaria")
myradio_map= radioButtons(inputId="Algorithmes_eval", label="Algorithms:",
                          list(  "Percentile" = "Percentile",
                                 "MinSan" = "MinSan",
                                 "C-SUM" = "Csum",
                                 "RDT+/fever Indicator" = "Ind"))
mycondparam_map_percentile=conditionalPanel(
  condition = "input.Algorithmes_eval == 'Percentile'",
  tags$hr(),
  tags$p("About this algorithm:"),
  helpText("An alert is triggered when the current week malaria cases exceeds 
           selected value of percentile of the whole chronological series of a site. 
           The 90th percentile is the value such that 90% of the time series values are below it (and therefore 10% are above). "),
  tags$hr(),
  sliderInput("Centile_map", 
              "Centile value (%):", 
              min = 1,
              max = 100, 
              value = 90
  ),
  tags$hr(),
  tags$p("This option is intended to improve the specificity of the alert system"),
  radioButtons(inputId="comet_map", label="Number of consecutive weeks above threshold:",
               c("1"=1 , "2"=2 , "3"= 3, "4"= 4),
               selected="3"
  )
)
mycondparam_map_minsan= conditionalPanel(
  condition = "input.Algorithmes_eval == 'MinSan'",
  tags$p(tags$strong("About algorithm")),
  helpText("The Ministry of Health, has defined a method based on slope calculation, by identify a doubling of the number of cases for 3 consecutive weeks. 
           They assume that a rapid multiplication of the number cases from week to week might signal onset of an epidemic."),
  sliderInput(inputId="slope_minsan", 
              "Slope Value:", 
              min = 1,
              max = 5, 
              step = 0.1,
              value = 2
  ),
  radioButtons(inputId="minsan_weekrange", "Number of weeks:",
               c("3" = "3","4" = "4")),
  checkboxInput(inputId="minsan_consecutive_week", label = "Consecutive Weeks", value = TRUE)
  
)
mycondparam_map_cumsum=conditionalPanel(
  condition = "input.Algorithmes_eval == 'Csum'",
  tags$hr(),
  tags$p(tags$strong("About algorithm")),
  helpText("The cumulative sum (C-SUM) method for epidemic detection is based on the construction of an average or base year by calculating the expected number of cases using the average for that week (and the previous and following week) during the past 5 years."),
  tags$hr(),
  sliderInput(inputId="Csum_year_map", 
              "Number of years:", 
              min = 2,
              max = 6, 
              step = 1,
              value = 5
  ),
  tags$hr(),
  
  
  sliderInput(inputId="Csum_week_map", "Weeks number:", 
              min = 1,
              max = 26, 
              value = 3),
  tags$hr(),
  sliderInput(inputId="Sd_csum_map", "standard deviation:", 
              min = 1,
              max = 10,
              step=0.01,
              value = 2),
  tags$hr(),
  tags$p("This option is intended to improve the specificity of the alert system"),
  radioButtons(inputId="week_Csum_map", "Number of consecutive weeks above threshold:",
               c("1"="1","2"="2","3" = "3"))
)
mycondparam_map_mcases=conditionalPanel(
  condition = "input.Algorithmes == 'Mcases'",
  selectInput("sites", "Sentinel Sites", choices = c("Farafangana","Ambovombe","Ambatondrazaka","Antsohihy","Anjozorobe","Antsirabe","Belo sur Tsiribihina","Behoririka","Ambato Boeny","Ambositra","Cd Andohatapenaka","Diego","Mandritsara","Edjeda","Fianarantsoa","Ihosy","Maevatanana","Morondava","Mahajanga","Miandrivazo","Manjakaray","Mananjary","Morombe","Moramanga","Maroantsetra","Maintirano","Nosy Be","Sambava","Sainte-Marie","Tsiroanomandidy","Tolagnaro","Toliary","Toamasina","Tsaralalana")),
  selected=("Farafangana"))
mycondparam_map_ind=  conditionalPanel(
  condition = "input.Algorithmes_eval == 'Ind'",
  br(),
  sliderInput(inputId = "exp_map", 
              "Malaria cases among fever cases:", 
              min = 0,
              max = 100, 
              value = 40
  ),
  sliderInput(inputId = "expC_map", 
              "malaria cases among visits:", 
              min = 0,
              max = 100, 
              value = 10
  ),
  br(),
  
  helpText("Malaria cases among fever cases. Indicator is considered, as high when ratio is greater than 40%, and malaria cases among consultants number is greater than 10%.")     
)
map_parameters=box(status = "primary", solidHeader = TRUE,
                   collapsible = TRUE,title="Parameters",width=4,
                  
                   myradio_map,
                  
                   mycondparam_map_percentile,
                   mycondparam_map_minsan,
                   mycondparam_map_cumsum,
                   mycondparam_map_mcases,
                   mycondparam_map_ind,
                   sentinel_sites
                  
)