#########################Visualization UI ##################################
# myhfi=list(
#                         checkboxInput(inputId="llin",label="LLIN",value = F),
#                         checkboxInput(inputId="irs",label="IRS",value = F),
#                         checkboxInput(inputId="ndvi",label="NDVI",value = F),
#                         checkboxInput(inputId="pmm",label="precipitation",value = F),
#                         checkboxInput(inputId="temp",label="Temperature",value = F))

algoviz_display=tabItem(tabName="myalgoviz",
                     fluidRow( 
                            showOutput("propsite_alerte", "highcharts")
                          # dygraphOutput("propsite_alerte"),
#                            box(status = "primary",title = "Check boxes to display time series:",
#                                solidHeader = TRUE,myhfi,width = 12)
                            ),
                     
                     sentinel_sites,
                     fluidRow( box(status = "primary", solidHeader = TRUE,
                                   collapsible = TRUE, plotlyOutput("malariacases"),width = 12)
#                                ,
#                                box(status = "primary", solidHeader = TRUE,
#                                    collapsible = TRUE,sentinel_sites,width = 3)
                     )
)