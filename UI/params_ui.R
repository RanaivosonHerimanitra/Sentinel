############################Main parameters UI ############################

##############################Level of aggregation main parameters #################                           
myfacies_algo=selectInput(inputId ="Cluster_algo", 
                          label="Data aggregation level (facies)",
                          list("National" = "Total",
                               "East"="East",
                               "South"="South", 
                               "Central High Land"="High_land",
                               "Fringe"="Fringe",
                               "National except East"="excepted_East",
                               "National except Central High Land"="excepted_High_land"))
##############################Disease to visualize#####################################
diseases_choices=selectInput(inputId = "diseases", 
                             label="Select a disease to monitor",
                             choices=list("Malaria" = "Malaria",
                                          "Diarrhea"="Diarrhea",
                                          "ILI"="ILI"
                             ),selected = "Malaria")
map_choices = radioButtons(inputId = "mapchoice",
                           label="Map type:",
                           #label = "Choose a map:",
                           list("Leaflet"="leaflet")
                                #,"Other"="other")
                           )
#####################radio button's choices for epidemic threshold's alerts:
#available algorithm when Malaria is displayed
algo_params1= conditionalPanel(
  #id="myalgo_params1",
  condition = "input.diseases=='Malaria'",
  radioButtons(inputId="Algorithmes_eval1", 
                          label="Algorithms:",
                          choices=list(  "Percentile" = "Percentile",
                                 "MinSan" = "MinSan",
                                 "C-SUM" = "Csum",
                                 "RDT+ among Fever cases" = "Ind")
                          ))
#available algorithm when Malaria is displayed
algo_params2 =conditionalPanel(
  #id="myalgo_params2",
  condition = "input.diseases!='Malaria'",
  radioButtons(inputId="Algorithmes_eval2", 
               label="Algorithms:",
               list(  "Percentile" = "Percentile",
                      "MinSan" = "MinSan",
                      "C-SUM" = "Csum"),
               selected = ""
  ))                          
myradio_map = list(algo_params1,algo_params2)                                 
########################################################################################
mycondparam_map_percentile=conditionalPanel(
  condition = "input.Algorithmes_eval1 == 'Percentile' | input.Algorithmes_eval2 == 'Percentile'",
  tags$hr(),
  tags$p("About this algorithm:"),
  helpText("An alert is triggered when disease cases number of last week exceed 
           selected percentile value of the whole time series of a site. 
           The 90th percentile is the value such that 90% of the time series values are below it (and therefore 10% are above). "),
  tags$hr(),
  sliderInput("Centile_map", 
              "Centile value (%):", 
              min = 1,
              max = 100, 
              value = 90
  ),
  tags$hr(),
  tags$p("This option is intended to improve the specificity of this early warning system:"),
  radioButtons(inputId="comet_map", 
               label="Number of consecutive weeks above threshold:",
               c("1"=1 , "2"=2 , "3"= 3, "4"= 4),
               selected="3"
  )
)
mycondparam_map_minsan= conditionalPanel(
  condition = "input.Algorithmes_eval1 == 'MinSan' | input.Algorithmes_eval2 == 'MinSan'",
  tags$p(tags$strong("About algorithm")),
  helpText("Malagasy Ministry of Health defined an algorithm which consists of identifying the duplication of the number of cases for 3 consecutive weeks (or not). 
           They assume that a rapid multiplication of the number cases from week to week might signal onset of an epidemic.  Default slope is set to 02 (doubling) but user can slide it."),
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
  condition = "input.Algorithmes_eval1 == 'Csum' | input.Algorithmes_eval2 == 'Csum'",
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
  tags$p("This option is intended to improve the specificity of this early warning system"),
  radioButtons(inputId="week_Csum_map", "Number of consecutive weeks above threshold:",
               c("1"="1","2"="2","3" = "3"))
)
mycondparam_map_mcases=conditionalPanel(
  condition = "input.Algorithmes == 'Mcases'",
  selectInput("sites", "Sentinel Sites", 
              choices = c("Farafangana","Ambovombe","Ambatondrazaka","Antsohihy","Anjozorobe","Antsirabe","Belo sur Tsiribihina","Behoririka","Ambato Boeny","Ambositra","Cd Andohatapenaka","Diego","Mandritsara","Edjeda","Fianarantsoa","Ihosy","Maevatanana","Morondava","Mahajanga","Miandrivazo","Manjakaray","Mananjary","Morombe","Moramanga","Maroantsetra","Maintirano","Nosy Be","Sambava","Sainte-Marie","Tsiroanomandidy","Tolagnaro","Toliary","Toamasina","Tsaralalana")),
  selected=("Farafangana"))
############## proportion indicator #Malaria/#fiever
mycondparam_map_ind=  conditionalPanel(
  condition = "input.Algorithmes_eval1 == 'Ind' & input.diseases=='Malaria'",
  br(),
  sliderInput(inputId = "exp_map", 
              label="Malaria cases among fever cases:", 
              min = 0,
              max = 100, 
              value = 40
  ),
  sliderInput(inputId = "expC_map", 
              label="Malaria cases among visits:", 
              min = 0,
              max = 100, 
              value = 10
  ),
  br(),
  helpText("Malaria cases among fever cases. Indicator is considered, as high when ratio is greater than 40%, and malaria cases among consultants number is greater than 10%.")     
)
###########################################################################
map_parameters=box(status = "primary",solidHeader = TRUE,
                   collapsible = TRUE,title="Parameters",
                   width=3,
                   myradio_map,
                   mycondparam_map_percentile,
                   mycondparam_map_minsan,
                   mycondparam_map_cumsum,
                   mycondparam_map_mcases,
                   mycondparam_map_ind)