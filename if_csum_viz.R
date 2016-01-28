if ( input$Algorithmes_eval=="Csum") {
  cat('You choose',input$Algorithmes_eval,'algorithm for data visualization...\n')
  if ( input$Cluster_algo=="Total" )
  {
    myprop=csum_algorithm()$propsite_alerte_csum
  } else {
    cat('You choose to display by:',input$Cluster_algo,"\n")
    mydata=preprocessing()
    myprop=calculate_csum(data=mydata,
                          Csum_year_map=input$Csum_year_map,
                          Csum_week_map=input$Csum_week_map,
                          Sd_csum_map=input$Sd_csum_map,
                          week_choice=input$week_choice,
                          week_Csum_map=input$week_Csum_map,
                          year_choice=input$year_choice,byvar="code")$propsite_alerte_csum_byfacies
  }
}