##############################Heatmap UI #################################
myheatmap=d3heatmapOutput("heatmap_percentile")
heatmap_legend_details=list(
  tags$p(tags$strong("Details:")),
  helpText("90th percentile is used to trigger alert (in red)."),
  helpText("Click and drag on the graph to zoom on the weeks."),
  helpText("Double-Click to resize the heatmap."),
  helpText("More options can be found on the corner right of the heatmap.")
)
heatmap_display = list(sliderInput(inputId = "nbyear",
                                   label = "Select number of years to visualize:",
                                   min=1,
                                   max=year(Sys.Date())-2007 + 1,
                                   ticks = T,
                                   step=1,
                                   value=1
                                  ),
                       myheatmap,
                       includeHTML("www/legende_heatmap.html"),
                       tags$strong("Alert-Normal-No Data"),
                       heatmap_legend_details)
