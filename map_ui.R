##########################Map UI #############################
individual_site_legend_details=list(
  tags$p(tags$strong("Details:")),
  helpText("90th percentile is used to trigger alert."),
  helpText("Click on the legend to hide/show variables"),
  helpText("Click and drag on the graph to zoom on the weeks."),
  helpText("More options can be found on the corner right of the graph.")
)
map_display=list(
  tags$h4("Week:",paste0( year(Sys.Date()) ,"_", isoweek(Sys.Date()) ) ),
  conditionalPanel( condition = "input.mapchoice=='leaflet'", 
                    leafletOutput("madagascar_map"),
                    includeHTML("www/legende.html"), #cool that works!
                    tags$strong("Alert -  Normal  -  No Data")
                    ),
  conditionalPanel( condition = "input.mapchoice=='other'", 
                    plotOutput("madagascar_map2",click = "madagascar_map2_click",
                               brush = brushOpts(id = "madagascar_map2_brush")),
                    tags$br(), tags$br(), tags$br(),tags$br(), tags$br(), tags$br(),tags$br(), tags$br(), tags$br(),
                    includeHTML("www/legende.html"),
                    tags$strong("Alert -  Normal  -  No Data")
                   ),
  tags$br(), tags$br(),
  plotlyOutput("weekly_disease_cases_persite"),
  individual_site_legend_details)

