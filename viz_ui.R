#########################Visualization UI ##################################

algoviz_display=tabItem(tabName="myalgoviz",
                        conditionalPanel(condition = "input.diseases=='Grippe'",
                        fluidRow(
                         #showOutput("ili_graph","highcharts"))
                         #rbokehOutput("ili_graph"))
                       plotlyOutput("ili_graph")),
                       tags$br(),
                       fluidRow(
                         plotlyOutput("propili"))
                        ),
                        conditionalPanel(condition = "input.diseases!='Grippe'",
                        fluidRow(
                            #showOutput("propsite_alerte", "highcharts"))
                            #rbokehOutput("propsite_alerte"))
                          plotlyOutput("propsite_alerte"))
                        ),
                        sentinel_sites,
                        fluidRow( box(status = "primary", solidHeader = TRUE,
                                   collapsible = TRUE, plotlyOutput("malariacases"),width = 12)
                     )
)