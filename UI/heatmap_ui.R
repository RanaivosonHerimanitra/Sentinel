##############################Heatmap UI #################################
myheatmap_alert_status=d3heatmapOutput("heatmap_alert_status")
myheatmap_perc_rank=d3heatmapOutput("heatmap_perc_rank")


heatmap_alert_status_legend_details=list(
  helpText("Click and drag on the graph to zoom on the weeks(Double-Click to unzoom)"),
  helpText("This Health heatmap of alert status can react to algorithms , sites and diseases. Feel free to modify these parameters"),
  helpText("More options can be found on the corner right of the heatmap."),
  tags$p(tags$strong("Legend:")),
  helpText("By default,90th percentile is used to trigger alert (in red).")
  
)

heatmap_perc_rank_legend_details=list(
  helpText("Click and drag on the graph to zoom on the weeks(Double-Click to unzoom)"),
  helpText("Health heatmap plot of percentile rank (value) that reacts to chosen disease:"),
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
                       tags$h2("Alert based heatmap",class="jumbotron"),
                       myheatmap_alert_status,
                       includeHTML("www/legende_heatmap.html"),
                       heatmap_alert_status_legend_details,
                       tags$h2("Percentile rank based heatmap",class="jumbotron"),
                       myheatmap_perc_rank,
                       heatmap_perc_rank_legend_details)
