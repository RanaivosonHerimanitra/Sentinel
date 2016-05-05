#######################Past alerts display in a Bubble chart ######################
legend_details_pastalert=list(
  helpText("Click on the legend to hide/show sites"),
  helpText("Click and drag on the graph to zoom"),
  tags$p(tags$strong("You can select different geographical aggregation (facies) and see real time updates")),
  helpText("By default,90th percentile and 03 consecutive weeks are used to trigger alerts"))

pastalert_display=list(plotlyOutput("mybubble"),legend_details_pastalert)