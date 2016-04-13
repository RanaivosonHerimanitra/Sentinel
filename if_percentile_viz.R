if (input$diseases=="Malaria")
{
  if ( input$Algorithmes_eval1=="Percentile") {
    cat('You choose',input$Algorithmes_eval,'algorithm for data visualization...\n')
    if ( input$Cluster_algo=="Total" )
    {
      myprop=percentile_algorithm()$propsite_alerte_percentile
    } else {
      cat('You choose to display by:',input$Cluster_algo,"\n")
      mydata=preprocessing()
      myprop=percentile_algorithm()$propsite_alerte_percentile_byfacies
      #effectively select facies that has been chosen by user:
      myprop=myprop[facies==input$Cluster_algo]
    }
    #choose to display >2010
    myprop=myprop[year(deb_sem)>2009]
  }
} else {
  if ( input$Algorithmes_eval2=="Percentile") {
    cat('You choose',input$Algorithmes_eval,'algorithm for data visualization...\n')
    if ( input$Cluster_algo=="Total" )
    {
      myprop=percentile_algorithm()$propsite_alerte_percentile
    } else {
      cat('You choose to display by:',input$Cluster_algo,"\n")
      mydata=preprocessing()
      myprop=percentile_algorithm()$propsite_alerte_percentile_byfacies
      #effectively select facies that has been chosen by user:
      myprop=myprop[facies==input$Cluster_algo]
      #effectively select facies that has been chosen by user:
      myprop=myprop[facies==input$Cluster_algo]
    }
    #choose to display >2010
    myprop=myprop[year(deb_sem)>2009]
  }
}
