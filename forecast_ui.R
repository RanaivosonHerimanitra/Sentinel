###########################Forecast UI ######################################
myforecast_ui=list(fluidRow(box(title="Choose a model for prediction",
                           width=3,
                           selectInput(inputId = "mymodel",
                                       label="Model",
                                       choices = list("ARMAX"="ARMAX",
                                                      "Holt Linear Trend"="HLT",
                                                      "Kalman-Filter"="KFilter",
                                                      "Random Forest"="RF",
                                                      "Combo Forecast"="Combo"),
                                       selected = "HLT")),
                           box(title="Choose a forecasting type",
                               width=3,
                               selectInput(inputId = "forecast_type",
                                           label="Forecast type",
                                           choices=list("Retrospective"="retrospective",
                                                        "Prospective"="prospective"))))
                   ,box(plotlyOutput("forecast_plot"),width=12) )