if ( input$Algorithmes_eval=="MinSan") {
  cat('You choose',input$Algorithmes_eval,'algorithm for data visualization...\n')
  if ( input$Cluster_algo =="Total" )
  {
    myprop=minsan_algorithm()$propsite_alerte_minsan
    
  } else {
    cat('You choose to display by:',input$Cluster_algo,"\n")
    mydata=preprocessing()
    myprop=minsan_algorithm()$propsite_alerte_minsan_byfacies
  }
  #choose to display >2010
  myprop=myprop[year(deb_sem)>2009]
  
}
