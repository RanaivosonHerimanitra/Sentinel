if ( input$diseases=="Malaria")
{
  if ( input$Algorithmes_eval1=="Ind") {
    cat('You choose',input$Algorithmes_eval,'algorithm for data visualization...\n')
    if ( input$Cluster_algo=="Total" )
    {
      myprop=tdrplus_fever_ind()$propsite_alerte_fever
    } else {
      myprop=tdrplus_fever_ind()$propsite_alerte_fever_byfacies
      #effectively select facies that has been chosen by user:
      myprop=myprop[facies==input$Cluster_algo]
    }
    #choose to display >2010
    myprop=myprop[year(deb_sem)>2009]
  }
} 