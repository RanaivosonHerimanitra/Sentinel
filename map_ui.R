##########################Map UI #############################
map_display=list(
  #leafletOutput("madagascar_map"), 
  plotOutput("madagascar_map",
             click = "madagascar_map_click",
             brush = brushOpts(
               id = "madagascar_map_brush"
             )),
  tags$br(),
  plotlyOutput("weekly_disease_cases_persite"))

