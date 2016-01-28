##########################Map UI #############################
map_display=list(
  #year_retrospective,
  #week_retrospective,
#   dateInput("date", "Select a date to visualize past alerts",
#             value = Sys.Date(),
#             weekstart = 1),
  leafletOutput("madagascar_map"), 
  tags$br(),
  plotlyOutput("weekly_disease_cases_persite"))

