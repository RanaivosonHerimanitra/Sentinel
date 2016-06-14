if ( input$diseases=="Malaria")
{
  source("csum.R")
  if ( input$Algorithmes_eval1=="Csum") {
    cat('You choose',input$Algorithmes_eval,'algorithm for data visualization...\n')
    if ( input$Cluster_algo=="Total" )
    {
      myprop=csum_algorithm()$propsite_alerte_csum
    } else {
      cat('You choose to display by:',input$Cluster_algo,"\n")
      mydata=preprocessing()
      myprop=csum_algorithm()$propsite_alerte_csum_byfacies
      #effectively select facies that has been chosen by user:
      myprop=myprop[facies==input$Cluster_algo]
    }
    #choose to display >2010
    myprop=myprop[year(as.Date(deb_sem,origin="1970-01-01"))>2009]
  }
} else {
  source("csum.R")
  if ( input$Algorithmes_eval2=="Csum") {
    cat('You choose',input$Algorithmes_eval,'algorithm for data visualization...\n')
    if ( input$Cluster_algo=="Total" )
    {
      myprop=csum_algorithm()$propsite_alerte_csum
    } else {
      cat('You choose to display by:',input$Cluster_algo,"\n")
      mydata=preprocessing()
      myprop=csum_algorithm()$propsite_alerte_csum_byfacies
      #effectively select facies that has been chosen by user:
      myprop=myprop[facies==input$Cluster_algo]
    }
    #choose to display >2010
    myprop=myprop[year(as.Date(deb_sem,origin="1970-01-01"))>2009]
  }
}
