###########################model choices ####################################
model_choices=box(title="Choose a model for prediction",
                  width=3,
                  selectInput(inputId = "mymodel",
                              label="Model",
                              choices = list("ARMAX (coming soon...)"="ARMAX",
                                             "Holt Linear Trend"="HLT",
                                             "Kalman-Filter (coming soon...)"="KFilter",
                                             "Random Forest (coming soon...)"="RF",
                                             "Combo Forecast (coming soon...)"="Combo"),
                              selected = "HLT"))

###########################model type :retro/prospe ##########################
model_type=box(title="Choose a forecasting type",
               width=3,
               selectInput(inputId = "forecast_type",
                           label="Forecast type",
                           choices=list("Retrospective"="retrospective",
                                        "Prospective"="prospective"),
                           selected="prospective"))
######################### performance monitoring ###########################
#performance_monitor=valueBox(value=12,icon="",color="red")
###########################Forecast UI (menu)################################
myforecast_ui=list(fluidRow(model_choices,model_type)
                   ,box(plotlyOutput("forecast_plot"),width=12) )