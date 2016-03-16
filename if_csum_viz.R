source("csum.R")
if ( input$Algorithmes_eval=="Csum") {
  cat('You choose',input$Algorithmes_eval,'algorithm for data visualization...\n')
  if ( input$Cluster_algo=="Total" )
  {
    myprop=csum_algorithm()$propsite_alerte_csum
  } else {
    cat('You choose to display by:',input$Cluster_algo,"\n")
    mydata=preprocessing()
    myprop=csum_algorithm()$propsite_alerte_csum_byfacies
  }
  #choose to display >2010
  myprop=myprop[year(deb_sem)>2009]
}