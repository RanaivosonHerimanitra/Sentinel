##############################Heatmap UI #################################
myheatmap=d3heatmapOutput("heatmap_percentile")
#myheatmap=plotlyOutput("heatmap_percentile")
heatmap_legend_details=list(
  helpText("Click and drag on the graph to zoom on the weeks(Double-Click to unzoom)"),
  helpText("More options can be found on the corner right of the heatmap."),
  tags$p(tags$strong("Legend:")),
  helpText("By default,90th percentile is used to trigger alert (in red).")
  
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
                       heatmap_legend_details)
