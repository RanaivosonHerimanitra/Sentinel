if ( input$Algorithmes_eval=="Ind") {
  cat('You choose',input$Algorithmes_eval,'algorithm for data visualization...\n')
  if ( input$Cluster_algo=="Total" )
  {
    myprop=tdrplus_fever_ind()$propsite_alerte_fever
  } else {
    myprop=tdrplus_fever_ind()$propsite_alerte_fever_byfacies
  }
}