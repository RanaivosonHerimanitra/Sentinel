##########################Map UI #############################
map_display=list(
  conditionalPanel( condition = "input.mapchoice=='leaflet'", leafletOutput("madagascar_map")),
  conditionalPanel( condition = "input.mapchoice=='other'", plotOutput("madagascar_map2",
                                                                       click = "madagascar_map2_click",
                                                                       brush = brushOpts(id = "madagascar_map2_brush"))
    ),
  tags$br(), tags$br(),
  plotlyOutput("weekly_disease_cases_persite"))

