##############################Heatmap UI #################################
myheatmap=d3heatmapOutput("heatmap_percentile")

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
                       tags$strong("Alert(=2)-Normal(=1)-No Data(=0)"))
