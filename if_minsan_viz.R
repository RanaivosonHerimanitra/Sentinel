if ( input$Algorithmes_eval=="MinSan") {
  cat('You choose',input$Algorithmes_eval,'algorithm for data visualization...\n')
  if ( input$Cluster_algo =="Total" )
  {
    myprop=minsan_algorithm()$propsite_alerte_minsan
  } else {
    cat('You choose to display by:',input$Cluster_algo,"\n")
    mydata=preprocessing()
    myprop=calculate_minsan(data=mydata,slope_minsan=input$slope_minsan,
                            minsan_weekrange=input$minsan_weekrange,
                            minsan_consecutive_week=input$minsan_consecutive_week,
                            byvar="code")$propsite_alerte_minsan_byfacies
  }
}