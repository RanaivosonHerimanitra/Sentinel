##########################Map UI #############################
map_display=list(
  leafletOutput("madagascar_map"), 
  tags$br(),
  plotlyOutput("weekly_disease_cases_persite"))

