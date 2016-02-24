#########################Visualization UI ##################################
# myhfi=list(
#                         checkboxInput(inputId="llin",label="LLIN",value = F),
#                         checkboxInput(inputId="irs",label="IRS",value = F),
#                         checkboxInput(inputId="ndvi",label="NDVI",value = F),
#                         checkboxInput(inputId="pmm",label="precipitation",value = F),
#                         checkboxInput(inputId="temp",label="Temperature",value = F))

algoviz_display=tabItem(tabName="myalgoviz",
                        conditionalPanel(condition = "input.diseases=='Grippe'",
                        fluidRow(
                         #showOutput("ili_graph","highcharts"))
                         #rbokehOutput("ili_graph"))
                       plotlyOutput("ili_graph"))
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