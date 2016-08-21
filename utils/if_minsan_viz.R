if ( input$diseases=="Malaria" )
{
  if ( input$Algorithmes_eval1=="MinSan") {
    cat('You choose',input$Algorithmes_eval,'algorithm for data visualization...\n')
    if ( input$Cluster_algo =="Total" )
    {
      myprop=minsan_algorithm()$propsite_alerte_minsan
      
    } else {
      cat('You choose to display by:',input$Cluster_algo,"\n")
      #mydata=preprocessing()
      mydata=fread(paste0("temp/",input$diseases,".csv"))
      myprop=minsan_algorithm()$propsite_alerte_minsan_byfacies
      #effectively select facies that has been chosen by user:
      myprop=myprop[facies==input$Cluster_algo]
    }
    #choose to display >2010
    myprop=myprop[year(as.Date(deb_sem,origin="1970-01-01"))>2009]
    
  }
  
} else {
  
  if ( input$Algorithmes_eval2=="MinSan") {
    cat('You choose',input$Algorithmes_eval,'algorithm for data visualization...\n')
    if ( input$Cluster_algo =="Total" )
    {
      myprop=minsan_algorithm()$propsite_alerte_minsan
      
    } else {
      cat('You choose to display by:',input$Cluster_algo,"\n")
      #mydata=preprocessing()
      mydata=fread(paste0("temp/",input$diseases,".csv"))
      myprop=minsan_algorithm()$propsite_alerte_minsan_byfacies
      #effectively select facies that has been chosen by user:
      myprop=myprop[facies==input$Cluster_algo]
    }
    #choose to display >2010
    myprop=myprop[year(as.Date(deb_sem,origin="1970-01-01"))>2009]
    
  }
}
