#########################Visualization UI ##################################
#################### detailed legend for Malaria ###########
malaria_legend_details=conditionalPanel(
  condition = "input.diseases == 'Malaria'",
  helpText("Click on the legend to hide/show variables"),
  helpText("Click and drag on the graph to zoom"),
  tags$p(tags$strong("Legend")),
  helpText("RDT+, Number of reported Malaria cases by epidemiological week"), 
  helpText("90th percentile (threshold)"))
#
algoviz_display=tabItem(tabName="myalgoviz",
                        conditionalPanel(condition = "input.diseases=='ILI'",
                        fluidRow(
                         plotlyOutput("ili_graph")),
                        tags$br(),
                       fluidRow(
                         plotlyOutput("propili"))
                        ),
                        conditionalPanel(condition = "input.diseases!='ILI'",
                        fluidRow(
                          plotlyOutput("propsite_alerte"))
                        
                        ),malaria_legend_details
                        #sentinel_sites,
                       # fluidRow(  plotlyOutput("malariacases") )
)