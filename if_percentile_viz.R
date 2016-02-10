if ( input$Algorithmes_eval=="Percentile") {
    cat('You choose',input$Algorithmes_eval,'algorithm for data visualization...\n')
  if ( input$Cluster_algo=="Total" )
  {
    myprop=percentile_algorithm()$propsite_alerte_percentile
  } else {
    cat('You choose to display by:',input$Cluster_algo,"\n")
    mydata=preprocessing()
    myprop=calculate_percentile(data=mydata,
                                week_length=input$comet_map,
                                percentile_value=input$Centile_map)$propsite_alerte_percentile_byfacies
    
  }
  #choose to display >2010
  myprop=myprop[year(deb_sem)>2009]
}