options(rcharts.cdn = TRUE)

bindEvent <- function(eventExpr, callback, env=parent.frame(), quoted=FALSE) {
  eventFunc <- exprToFunction(eventExpr, env, quoted)
  
  initialized <- FALSE
  invisible(observe({
    eventVal <- eventFunc()
    if (!initialized)
      initialized <<- TRUE
    else
      isolate(callback())
  }))
}







shinyServer(function(input, output,session) {
  
  
  #to output progress of import1.R
  withProgress(message = 'Please wait while server is requesting - Part 1', value = 0.1, {   
    cat('reading data from source database\n')
    source("import1.r")  
    
    for (i in 1:1) {
      
      # Increment the progress bar, and update the detail text.
      incProgress(0.1 , detail = paste("part 1 completed")) } 
    incProgress(0.5)
    setProgress(1)
  })
  #to output progress of import12.R
  withProgress(message = 'Please wait while server is requesting - Part 2', value = 0.25, {   
    cat('cleaning data\n')
    source("import12.r")  
    
    for (i in 1:1) {
      
      # Increment the progress bar, and update the detail text.
      incProgress(0.1 , detail = paste("part 2 completed")) } 
      incProgress(0.5)
      setProgress(1)
  })
  
  #to output progress of import2.R
  withProgress(message = 'Please wait while server is requesting - Part 3', value = 0.70, {   
    source("import2.r")  
    for (i in 1:1) {
      # Increment the progress bar, and update the detail text.
      incProgress(0.1 , detail = paste("part 3 completed")) } 
      incProgress(0.5)
      setProgress(1)
  })
  #to output progress of import4.R
  withProgress(message = 'Please wait while server is requesting Part 4', value = 0.8, {   
    source("import3.r")  
    for (i in 1:1) { 
      # Increment the progress bar, and update the detail text.
      incProgress(0.1 , detail = paste("part 4 completed")) } 
      incProgress(0.5)
      setProgress(1)
  })
  
  
  makeReactiveBinding('selectedCity')
  
  observe({
    
    # Percentile --------------------------------------------------------------
    
    # Cartographie ------------------------------------------------------------
    
    # Percentile_Map ---------------------------------------------------------- 
    
    color_frame=Percentile
    
    Colorcities_percentile <- reactive({
      
      if (input$comet_map=="1"){
        color_frame<- color_frame > input$Centile_map
#         color_frame_1<-as.data.frame(color_frame)
      }
      
      if (input$comet_map=="2"){
        color_frame = lag(color_frame,0) > input$Centile_map & lag(color_frame,1) > input$Centile_map
#         color_frame_1<-as.data.frame(color_frame)
      }
      
      if (input$comet_map=="3"){
        color_frame = lag(color_frame,0) > input$Centile_map & lag(color_frame,1) > input$Centile_map & lag(color_frame,2) > input$Centile_map
#         color_frame_1<-as.data.frame(color_frame)
      }
      
      if (input$comet_map=="4"){
        color_frame = lag(color_frame,0) > input$Centile_map & lag(color_frame,1) > input$Centile_map & lag(color_frame,2) > input$Centile_map & lag(color_frame,3) > input$Centile_map
#         color_frame_1<-as.data.frame(color_frame)
      }  
      #no need to repeat each time as.data.frame
      #no performance gain, just for readability
      #no need to also rename the dataframe:

#       color_frame_1<-as.data.frame(color_frame)
#       color_frame_1<-last(color_frame_1)
#       color_frame_1<-as.data.frame(t(color_frame_1))
#       colnames(color_frame_1)<-c("color")
#       
#       
#       color_frame_1[color_frame_1==TRUE]<-c("#e40e09")
#       color_frame_1[color_frame_1==FALSE]<-c("#36a90f")
#       color_frame_1[is.na(color_frame_1)]<-c("#898980")
      #color_frame_1<-as.data.frame(color_frame_1,stringsAsFactors = FALSE)

#color_frame<-as.data.frame(color_frame)
color_frame<-last(color_frame)
color_frame<-as.data.frame(t(color_frame))
colnames(color_frame)<-c("color")


color_frame[color_frame==TRUE]<-c("#e40e09")
color_frame[color_frame==FALSE]<-c("#36a90f")
color_frame[is.na(color_frame)]<-c("#898980")
      return(color_frame)
    })
    
    ind_map<-reactive({
      results_indR<-Data_indR > input$exp_map
      results_indC<-Data_indC > input$expC_map
      Fever_ind<-results_indR * results_indC
      color_frame_ind<-last(Fever_ind)
      color_frame_ind<-as.data.frame(t(color_frame_ind))
      colnames(color_frame_ind)<-c("color")   
      color_frame_ind[color_frame_ind==1]<-c("#e40e09")
      color_frame_ind[color_frame_ind==0]<-c("#36a90f")
      color_frame_ind[is.na(color_frame_ind)]<-c("#898980")
      color_frame_ind<-as.data.frame(color_frame_ind,stringsAsFactors = FALSE)
      return(color_frame_ind)
    })
    
    
    
    f7 <- function(x){ tmp<-cumsum(x);tmp-cummax((!x)*tmp)}
    
    csum_map<-reactive({     
      results.cluster=ref
      comparative=last_year
      
      
      Data_mean<-rollapply(results.cluster,input$Csum_week_map,mean,na.rm=TRUE,align="center")
      array_lag <-last(Data_mean,input$Csum_year_map*52)
      lst <- lapply(seq(1,length(array_lag[,1]), by=52), function(i) array_lag[i:(i+51),])
      Data_array <- array(unlist(lst), dim=c(52,length(results.cluster[1,]),(length(results.cluster[,1])/52)))
      array_lag_sum<-apply(Data_array,c(1,2),FUN=function(x){sum(x,na.rm=TRUE)/sum(!is.na(x))})
      array_lag_sd<-apply(Data_array,c(1,2),FUN=function(x){sd(x,na.rm=TRUE)})
      C_sum_results<-array_lag_sum +(input$Sd_csum_map* array_lag_sd)
      C_sum_results_t<-last_year > C_sum_results
      C_sum_results_na<-last_year > C_sum_results
      C_sum_results_t[is.na(C_sum_results_t)]<-FALSE
      C_sum_results_t <- f7(C_sum_results_t)
      C_sum_results_t<-C_sum_results_t>= input$week_Csum_map  
      C_sum_results_t[is.na(C_sum_results_na)]<-NA
      C_sum_results_t<-last(C_sum_results_t)
      C_sum_results_t<-as.data.frame(t(C_sum_results_t))
      colnames(C_sum_results_t)<-c("color")  
      C_sum_results_t[C_sum_results_t==TRUE]<-c("#e40e09")
      C_sum_results_t[C_sum_results_t==FALSE]<-c("#36a90f")
      C_sum_results_t[is.na(C_sum_results_t)]<-c("#898980")
      C_sum_results_t<-as.data.frame(C_sum_results_t,stringsAsFactors = FALSE)
      return(C_sum_results_t)
    })     
    
    
    
    
    Minsan_map<-reactive({
      
      results.cluster=PaluConf
      
      
      if (input$consecutiv_map=="TRUE"){  
        if (input$Week_slope_map=="3") {
          results.cluster = (lag(results.cluster,0) > input$MinSan_map*lag(results.cluster,1)) & (lag(results.cluster,1) > input$MinSan_map*lag(results.cluster,2))}
        else 
          if (input$Week_slope_map=="4"){ results.cluster = (lag(results.cluster,0) > input$MinSan_map*lag(results.cluster,1)) & (lag(results.cluster,1) > input$MinSan_map*lag(results.cluster,2)) & (lag(results.cluster,2) > input$MinSan_map*lag(results.cluster,3)) }
      } 
      
      else 
        if(input$consecutiv_map=="FALSE"){  
          if (input$Week_slope_map=="3") {
            results.cluster = (lag(results.cluster,0) > input$MinSan_map*lag(results.cluster,2))}
          else 
            if (input$Week_slope_map=="4"){ 
              results.cluster = (lag(results.cluster,0) > input$MinSan_map*lag(results.cluster,3)) }
        }
      
      results.cluster<-last(results.cluster)
      results.cluster<-as.data.frame(t(results.cluster))
      colnames(results.cluster)<-c("color")   
      results.cluster[results.cluster==TRUE]<-c("#e40e09")
      results.cluster[results.cluster==FALSE]<-c("#36a90f")
      results.cluster[is.na(results.cluster)]<-c("#898980")
      results.cluster<-as.data.frame(results.cluster,stringsAsFactors = FALSE)
      return(results.cluster)
    })
    
    
    
    color_mapping<-reactive({
      if(input$Algorithmes_eval=='Percentile'){
        color_maping=Colorcities_percentile()
      } else if(input$Algorithmes_eval=='Ind'){
        color_maping=ind_map()
      } else if(input$Algorithmes_eval=='MinSan'){
        color_maping=Minsan_map()
      } else if(input$Algorithmes_eval=='Csum'){
        color_maping=csum_map()
      } 
    })
    
    # Cartographie ------------------------------------------------------------
    
    
    # carto_percentile --------------------------------------------------------
    
    
    ref_time_map<-reactive({strftime(index(PaluConf), format="%Y-%U")})
    popCol <- reactive({names(rev(cases_sig)[1])})

    cat('merging sentinel_latlong with color mapping\n') 
    #color_mapping=color_mapping()
    sentinel_latlong<-merge(sentinel_latlong, color_mapping(), by.x ="CODE", by.y = 0)
    cat('after merging\n')
    fg<-Percentile
    fg[fg<60]<-10
    fg[fg<85]<-40
    fg[fg>90]<-100
    Percentile_map<-t(fg)
    Percentile_map<-as.data.frame(Percentile_map)
    
    cases_plot<-reactive({
      if(input$Algorithmes_eval=='Percentile'){
        cases_plot=Percentile_map
      } else {
        cases_plot=cases
      }
      #no need of an else
#       } else if(input$Algorithmes_eval=='Ind'){
#         cases_plot=cases
#       } else if(input$Algorithmes_eval=='MinSan'){
#         cases_plot=cases
#       } else if(input$Algorithmes_eval=='Csum'){
#         cases_plot=cases
      } 
    })
    
    
    cases_sig_plot <- merge(sentinel_latlong, cases_plot(), by.x ="CODE", by.y = 0)
    cases_sig <- merge(sentinel_latlong, cases, by.x ="CODE", by.y = 0)
    
    
    taille<-length(cases_sig)  
    popSeries <- function(cases_sig) {
      as.vector(colSums(cases_sig[6:taille],na.rm=TRUE))}
    
    
   
    
    topCitiesInBounds_plot <- reactive({
      citie_plot <- cases_sig_plot
      
    })
    
    topCitiesInBounds <- reactive({
      cities <- cases_sig
      
    })
    
   
    
    # Create the map; this is not the "real" map, but rather a proxy
    # object that lets us control the leaflet map on the page.
    map <- createLeafletMap(session, 'map')
    
    observe({
      if (is.null(input$map_click))
        return()
      selectedCity <<- NULL
    })
    
    radiusF<-reactive({
      #no need of multiple conditions
       if ( input$Algorithmes_eval=="Percentile") {
         radiusF=15000
       } else {
         radiusF=50000
       }
#       if(input$Algorithmes_eval=='Percentile'){
#         radiusF=15000
#       } else if(input$Algorithmes_eval=='Ind'){
#         radiusF=50000
#       } else if(input$Algorithmes_eval=='MinSan'){
#         radiusF=50000
#       } else if(input$Algorithmes_eval=='Csum'){
#         radiusF=50000
#       } 
    })
    
    
    
    
    
    #     
    #      radiusFactor <- 50000
    
    observe({
      map$clearShapes()
      cities <- topCitiesInBounds_plot()
      if (nrow(cities) == 0)
        return()
      map$addCircle(
        cities$Lat,
        cities$Long,
        cities[[names(rev(cases_sig_plot)[1])]] * radiusF() / max(2, input$map_zoom)^2,
        row.names(cities),
        list(
          weight=1,
          fill=TRUE,
          color=cities$color
          
        )
      )
    })
    
    
    observe({
      event <- input$map_shape_click
      if (is.null(event))
        return()
      map$clearPopups()
      
      isolate({
        cities <- topCitiesInBounds()
        city <- cities[row.names(cities) == event$id,]
        selectedCity <<- city
        content <- as.character(tagList(
          tags$strong(paste(city$name, city$CODE)),
          tags$br(),
          sprintf("Estimated population, %s:", input$year),
          tags$br(),
          prettyNum(city[[popCol()]], big.mark=',')
        ))
        #          map$showPopup(event$lat, event$lng, content, event$id)
      })
    })
    
   
    output$cityTimeSeriesLabel <- renderText({
      if (is.null(selectedCity)) { 
        paste("Current week :",sxc)
      } else {
        paste('cases of',selectedCity$name,',',selectedCity$CODE, sxc)
      }
    })
    
    output$cityTimeSeries <- renderChart2({
      refi_time<-ref_week
      cities <- NULL
      if (!is.null(selectedCity))
        cities <- selectedCity
      
      extr<-paste(selectedCity$CODE)
      test<-pmm[,(extr)]
      test<-as.data.frame(test)
      test<-round(test,0)
      colnames(test)<-("pmm")
      
      extr_mild<-paste(selectedCity$CODE)
      mild_map<-mild_carto[,(extr_mild)]
      mild_map<-as.data.frame(mild_map)
      colnames(mild_map)<-("MILD")
      mild_map<-as.data.frame(mild_map)
      row.names(mild_map)<-row.names(test$pmm)
      
      
      
      popData <- popSeries(cities)
      
      df <- data.frame(year = refi_time, pop = popData)      
     
      
      h8 <- Highcharts$new()
      h8$xAxis(type='datetime',categories=ref_week, tickInterval = 30)
      
      h8$yAxis( list(list(title = list(text = 'Rainfall (mm)'),min=0)
                     , list(title = list(text = 'Cases'), opposite = TRUE,min=0)))
      
      h8$series(name = 'Cases', type = 'line', color = '#cc3333',
                data = df$pop, yAxis = 1)
      
            h8$series(name = 'LLIN', type = 'area', color = '#678c9f',
                      data = mild_map$MILD, min = 0,visible=FALSE)
            
            h8$series(name = 'Rainfall', type = 'line', color = '#1e90ff',
                      data = test$pmm,visible=FALSE)
      
      h8$chart(zoomType = "x1")
      
      h8$set(dom = "cityTimeSeries")
      
      return(h8)
      
      
      
    })
    
    
    
    
    
    
    
    output$image2 <- renderImage({
      
      if(input$Algorithmes_eval=='Percentile'){
        return(list(
          src = "/srv/shiny-server/sentinel/www/legend_percentile1.png",
          contentType = "image/png"))
      } else {
        return(list(
                    src = "/srv/shiny-server/sentinel/www/legend1.png",
                    filetype = "image/jpeg",
                    alt = "This is a chainring"))
                  
      }
      #no need to repeat:
#       } else if(input$Algorithmes_eval=='Ind'){
#         return(list(
#           src = "/srv/shiny-server/sentinel/www/legend1.png",
#           filetype = "image/jpeg",
#           alt = "This is a chainring"
#         ))
#       } else if(input$Algorithmes_eval=='MinSan'){
#         return(list(
#           src = "/srv/shiny-server/sentinel/www/legend1.png",
#           filetype = "image/jpeg",
#           alt = "This is a chainring"
#         ))
#       } else if(input$Algorithmes_eval=='Csum'){
#         return(list(
#           src = "/srv/shiny-server/sentinel/www/legend1.png",
#           filetype = "image/jpeg",
#           alt = "This is a chainring"
#         ))
      }
    }, deleteFile = FALSE)
    
    
    
    
    output$myPlot<- renderChart2({
      
      if (input$Cluster=="East")                  { (Data_sentinel=exo_East)}
      if (input$Cluster=="High_land")             { (Data_sentinel=exo_High_land)}
      if (input$Cluster=="South")                 { (Data_sentinel=exo_South)}
      if (input$Cluster=="Fringe")                { (Data_sentinel=exo_Fringe)}
      if (input$Cluster=="excepted_East")         { (Data_sentinel=exo_No_East)}
      if (input$Cluster=="excepted_High_land")    { (Data_sentinel=exo_No_Hgl)}
      if (input$Cluster=="Total")                 { (Data_sentinel=exo_National)}
      
      #no need of an if statement here
      IslandParts=c("East","High_land","South","Fringe","excepted_East","excepted_High_land","Total")
      if (input$Cluster %in% IslandParts ) { 
        results.cluster=Percentile[ ,input$Cluster] 
      }

#       if (input$Cluster=="East")                  { (results.cluster=Percentile[ ,East])}
#       if (input$Cluster=="High_land")             { (results.cluster=Percentile[ ,High_land])}
#       if (input$Cluster=="South")                 { (results.cluster=Percentile[ ,South])}
#       if (input$Cluster=="Fringe")                { (results.cluster=Percentile[ ,Fringe])}
#       if (input$Cluster=="excepted_East")         { (results.cluster=Percentile[ ,excepted_East])}
#       if (input$Cluster=="excepted_High_land")    { (results.cluster=Percentile[ ,excepted_High_land])}
#       if (input$Cluster=="Total")                 { (results.cluster=Percentile[ ,Total])}
      
      
      
      if (input$comet=="1"){
        results.cluster<- results.cluster > input$Centile
        results.cluster<-as.data.frame(results.cluster)
        
        Data_sentinel<-as.data.frame(Data_sentinel)
        results.cluster<-apply(results.cluster,1,sum, na.rm=TRUE)
        results.cluster<-as.data.frame(results.cluster)
        results.cluster<-round((results.cluster$results.cluster/ Data_sentinel$sites_actifs)*100,0)
        results.cluster<-as.data.frame(results.cluster)     
        Data_sentinel<-cbind(Data_sentinel,results.cluster)
        Data_sentinel<-tail(Data_sentinel,input$daterange_percent*52)
        axis_graph<-tail(ref_week,input$daterange_percent*52)}
      
      if (input$comet=="2"){
        results.cluster = lag(results.cluster,0) > input$Centile & lag(results.cluster,1) > input$Centile
        Data_sentinel<-as.data.frame(Data_sentinel)
        results.cluster<-apply(results.cluster,1,sum, na.rm=TRUE)
        results.cluster<-as.data.frame(results.cluster)
        results.cluster<-round((results.cluster$results.cluster/ Data_sentinel$sites_actifs)*100,0)
        results.cluster<-as.data.frame(results.cluster)     
        Data_sentinel<-cbind(Data_sentinel,results.cluster)
        Data_sentinel<-tail(Data_sentinel,input$daterange_percent*52)
        axis_graph<-tail(ref_week,input$daterange_percent*52)}
      
      if (input$comet=="3"){
        results.cluster = lag(results.cluster,0) > input$Centile & lag(results.cluster,1) > input$Centile & lag(results.cluster,2) > input$Centile
        Data_sentinel<-as.data.frame(Data_sentinel)
        results.cluster<-apply(results.cluster,1,sum, na.rm=TRUE)
        results.cluster<-as.data.frame(results.cluster)
        results.cluster<-round((results.cluster$results.cluster/ Data_sentinel$sites_actifs)*100,0)
        results.cluster<-as.data.frame(results.cluster)     
        Data_sentinel<-cbind(Data_sentinel,results.cluster)
        Data_sentinel<-tail(Data_sentinel,input$daterange_percent*52)
        axis_graph<-tail(ref_week,input$daterange_percent*52)}
      
      if (input$comet=="4"){
        results.cluster = lag(results.cluster,0) > input$Centile & lag(results.cluster,1) > input$Centile & lag(results.cluster,2) > input$Centile & lag(results.cluster,3) > input$Centile
        Data_sentinel<-as.data.frame(Data_sentinel)
        results.cluster<-apply(results.cluster,1,sum, na.rm=TRUE)
        results.cluster<-as.data.frame(results.cluster)
        results.cluster<-round((results.cluster$results.cluster/ Data_sentinel$sites_actifs)*100,0)
        results.cluster<-as.data.frame(results.cluster)     
        Data_sentinel<-cbind(Data_sentinel,results.cluster)
        Data_sentinel<-tail(Data_sentinel,input$daterange_percent*52)
        axis_graph<-tail(ref_week,input$daterange_percent*52)}
      
      
      h1 <- Highcharts$new()
      h1$series(data = Data_sentinel$caid, type="area",name = "IRS", enabled=F, visible=FALSE)
      h1$series(data = Data_sentinel$mild, type="area",name = "LLIN", visible=FALSE)
      h1$series(data = Data_sentinel$pmm, type="line", name = "Rainfall", visible=FALSE)
      h1$series(data = Data_sentinel$lst, type="line", name = "Temperature", visible=FALSE) 
      h1$series(data = Data_sentinel$ndvi,  type='line', name = "Ndvi", visible=FALSE)
      h1$series(data = Data_sentinel$results.cluster, type="line",name = "Alert",enabled=T,width=0.5)
      h1$colors('rgba(52,73,94, 1.0)', 'rgba(41,128,185,1.0)','rgba(122,175,255,.9)','rgba(255,117,71 ,.9)',
                'rgba(22,160,133,1.0)','rgba(255,0,0,.9)')
      h1$legend(symbolWidth = 30)
      h1$yAxis( min = 0)
      h1$plotOptions(line=list(marker=list(enabled = F)),area=list(marker=list(enabled = F)))
      h1$chart(zoomType = "x1")
      h1$set(dom = "myPlot")
      h1$exporting(enabled = F)
      h1$xAxis(type='datetime', categories=axis_graph, tickInterval = 30)
      return(h1)
      
    })  
    
    
    # Minsan ------------------------------------------------------------------
    
    
    
    output$myMinSan<- renderChart2({
      
      if (input$Cluster_Min=="East")                  { (Data_sentinel=exo_East)}
      if (input$Cluster_Min=="High_land")             { (Data_sentinel=exo_High_land)}
      if (input$Cluster_Min=="South")                 { (Data_sentinel=exo_South)}
      if (input$Cluster_Min=="Fringe")                { (Data_sentinel=exo_Fringe)}
      if (input$Cluster_Min=="excepted_East")         { (Data_sentinel=exo_No_East)}
      if (input$Cluster_Min=="excepted_High_land")    { (Data_sentinel=exo_No_Hgl)}
      if (input$Cluster_Min=="Total")                 { (Data_sentinel=exo_National)}
      
      # no need of an if statement here
      IslandParts=c("East","High_land","South","Fringe","excepted_East","excepted_High_land","Total")
      if (input$Cluster %in% IslandParts ) { results.cluster=PaluConf[ ,input$Cluster_Min] }
      
#       if (input$Cluster_Min=="East")                  { (results.cluster=PaluConf[ ,East])}
#       if (input$Cluster_Min=="High_land")             { (results.cluster=PaluConf[ ,High_land])}
#       if (input$Cluster_Min=="South")                 { (results.cluster=PaluConf[ ,South])}
#       if (input$Cluster_Min=="Fringe")                { (results.cluster=PaluConf[ ,Fringe])}
#       if (input$Cluster_Min=="excepted_East")         { (results.cluster=PaluConf[ ,excepted_East])}
#       if (input$Cluster_Min=="excepted_High_land")    { (results.cluster=PaluConf[ ,excepted_High_land])}
#       if (input$Cluster_Min=="Total")                 { (results.cluster=PaluConf[ ,Total])}
      
      
      if (input$consecutiv=="TRUE"){  
        if (input$Week_slope=="3") {
          results.cluster = (lag(results.cluster,0) > input$MinSan*lag(results.cluster,1)) & (lag(results.cluster,1) > input$MinSan*lag(results.cluster,2))}
        else 
          if (input$Week_slope=="4"){ results.cluster = (lag(results.cluster,0) > input$MinSan*lag(results.cluster,1)) & (lag(results.cluster,1) > input$MinSan*lag(results.cluster,2)) & (lag(results.cluster,2) > input$MinSan*lag(results.cluster,3)) }
      } 
      
      else 
        if(input$consecutiv=="FALSE"){  
          if (input$Week_slope=="3") {
            results.cluster = (lag(results.cluster,0) > input$MinSan*lag(results.cluster,2))}
          else 
            if (input$Week_slope=="4"){ 
              results.cluster = (lag(results.cluster,0) > input$MinSan*lag(results.cluster,3)) }}
      
      
      Data_sentinel<-as.data.frame(Data_sentinel)
      results.cluster<-apply(results.cluster,1,sum, na.rm=TRUE)
      results.cluster<-as.data.frame(results.cluster)
      results.cluster<-round((results.cluster$results.cluster/ Data_sentinel$sites_actifs)*100,0)
      results.cluster<-as.data.frame(results.cluster)     
      Data_sentinel<-cbind(Data_sentinel,results.cluster)
      Data_sentinel<-tail(Data_sentinel,input$daterange_MinSan*52)
      axis_graph<-tail(ref_week,input$daterange_MinSan*52)
      
      
      h2 <- Highcharts$new()
      h2$series(data = Data_sentinel$caid, type="area",name = "IRS", enabled=F, visible=FALSE)
      h2$series(data = Data_sentinel$mild, type="area",name = "LLIN", visible=FALSE)
      h2$series(data = Data_sentinel$pmm, type="line", name = "Rainfall", visible=FALSE)
      h2$series(data = Data_sentinel$lst, type="line", name = "Temperature", visible=FALSE) 
      h2$series(data = Data_sentinel$ndvi,  type='line', name = "Ndvi", visible=FALSE)
      h2$series(data = Data_sentinel$results.cluster, type="line",name = "Alert",enabled=T,width=0.5)
      h2$colors('rgba(52,73,94, 1.0)', 'rgba(41,128,185,1.0)','rgba(122,175,255,.9)','rgba(255,117,71 ,.9)','rgba(22,160,133,1.0)','rgba(255,0,0,.9)')
      h2$yAxis( min = 0)
      h2$legend(symbolWidth = 30)
      h2$plotOptions(line=list(marker=list(enabled = F)),area=list(marker=list(enabled = F)))
      h2$chart(zoomType = "x1")
      h2$set(dom = "myMinSan")
      h2$exporting(enabled = F)
      h2$xAxis(type='datetime', categories=axis_graph, tickInterval = 30)
      return(h2)
      
    })
    
    
    
    # Cumulative Sum ----------------------------------------------------------
    
    output$myCsum<- renderChart2({
      
      if (input$Cluster_csum=="East")                  { (Data_sentinel=exo_East)}
      if (input$Cluster_csum=="High_land")             { (Data_sentinel=exo_High_land)}
      if (input$Cluster_csum=="South")                 { (Data_sentinel=exo_South)}
      if (input$Cluster_csum=="Fringe")                { (Data_sentinel=exo_Fringe)}
      if (input$Cluster_csum=="excepted_East")         { (Data_sentinel=exo_No_East)}
      if (input$Cluster_csum=="excepted_High_land")    { (Data_sentinel=exo_No_Hgl)}
      if (input$Cluster_csum=="Total")                 { (Data_sentinel=exo_National)}
      
      # no need of an if statement here
      IslandParts=c("East","High_land","South","Fringe","excepted_East","excepted_High_land","Total")
      if (input$Cluster_csum %in% IslandParts ) 
      { 
        results.cluster=ref[ ,input$Cluster_csum] 
        last_year[ ,input$Cluster_csum]
      }
      
#       if (input$Cluster_csum=="East")                  { (results.cluster=ref[ ,East])}
#       if (input$Cluster_csum=="High_land")             { (results.cluster=ref[ ,High_land])}
#       if (input$Cluster_csum=="South")                 { (results.cluster=ref[ ,South])}
#       if (input$Cluster_csum=="Fringe")                { (results.cluster=ref[ ,Fringe])}
#       if (input$Cluster_csum=="excepted_East")         { (results.cluster=ref[ ,excepted_East])}
#       if (input$Cluster_csum=="excepted_High_land")    { (results.cluster=ref[ ,excepted_High_land])}
#       if (input$Cluster_csum=="Total")                 { (results.cluster=ref[ ,Total])}
      
      
#       if (input$Cluster_csum=="East")                  { (comparative=last_year[ ,East])}
#       if (input$Cluster_csum=="High_land")             { (comparative=last_year[ ,High_land])}
#       if (input$Cluster_csum=="South")                 { (comparative=last_year[ ,South])}
#       if (input$Cluster_csum=="Fringe")                { (comparative=last_year[ ,Fringe])}
#       if (input$Cluster_csum=="excepted_East")         { (comparative=last_year[ ,excepted_East])}
#       if (input$Cluster_csum=="excepted_High_land")    { (comparative=last_year[ ,excepted_High_land])}
#       if (input$Cluster_csum=="Total")                 { (comparative=last_year[ ,Total])}
      
      
      Data_mean<-rollapply(results.cluster,input$Csum_week,mean,na.rm=TRUE,align="center")
      array_lag <-last(Data_mean,input$Csum_year*52)
      lst <- lapply(seq(1,length(array_lag[,1]), by=52), function(i) array_lag[i:(i+51),])
      Data_array <- array(unlist(lst), dim=c(52,length(results.cluster[1,]),(length(results.cluster[,1])/52)))
      array_lag_sum<-apply(Data_array,c(1,2),FUN=function(x){sum(x,na.rm=TRUE)/sum(!is.na(x))})
      array_lag_sd<-apply(Data_array,c(1,2),FUN=function(x){sd(x,na.rm=TRUE)})
      C_sum_results<-comparative > (array_lag_sum +(input$Sd_csum* array_lag_sd))
      drop_csum<-c("mrb")
      C_sum_results<-C_sum_results[,!(names(C_sum_results) %in% drop_csum)]
      ref_results<-strftime(index(C_sum_results),format="%Y-%m-%d") 
      
      plofunction_csum = function (C_sum_results) {
        firstone <- which(C_sum_results == 0)[1]
        subindex <- C_sum_results[firstone:length(C_sum_results)] == 0
        result <- c(rep(NA,firstone-1),1:length(subindex) - which(subindex)[cumsum(subindex)]) 
        return(result)
      }
      C_sum_results<-as.data.frame(apply(C_sum_results,2,plofunction_csum))
      C_sum_results[C_sum_results<= input$week_Csum]<-0
      C_sum_results[C_sum_results>= input$week_Csum]<-1  
      row.names(C_sum_results)<-ref_results 
      C_sum<-apply(C_sum_results,1,sum, na.rm=TRUE)
      C_sum<-as.xts(C_sum)     
      Data_sentinel<-as.xts(Data_sentinel)
      C_sum_merge<-merge(Data_sentinel,C_sum)
      C_sum_merge<-last(C_sum_merge,52)
      C_sum_merge$ratio<-round((C_sum_merge$C_sum/C_sum_merge$sites_actifs)*100,0)
      C_sum_merge<-as.data.frame(C_sum_merge)
      axis_graph<-tail(ref_week,52)
      
      
      h3 <- Highcharts$new()
      h3$series(data = C_sum_merge$caid, type="area",name = "IRS", enabled=F, visible=FALSE)
      h3$series(data = C_sum_merge$mild, type="area",name = "LLIN", visible=FALSE)
      h3$series(data = C_sum_merge$pmm, type="line", name = "Rainfall", visible=FALSE)
      h3$series(data = C_sum_merge$lst, type="line", name = "Temperature", visible=FALSE) 
      h3$series(data = C_sum_merge$ndvi,  type='line', name = "Ndvi", visible=FALSE)
      h3$series(data = C_sum_merge$ratio, type="line",name = "Alert",enabled=T,width=0.5)
      h3$colors('rgba(52,73,94, 1.0)', 'rgba(41,128,185,1.0)','rgba(122,175,255,.9)','rgba(255,117,71 ,.9)','rgba(22,160,133,1.0)','rgba(255,0,0,.9)')
      h3$yAxis( min = 0)
      h3$legend(symbolWidth = 30)
      h3$plotOptions(line=list(marker=list(enabled = F)),area=list(marker=list(enabled = F)))
      h3$chart(zoomType = "x1")
      h3$set(dom = "myCsum")
      h3$exporting(enabled = F)
      h3$xAxis(type='datetime', categories=axis_graph, tickInterval = 30)
      return(h3)
    })
    
    
    # Indicateur febrile ------------------------------------------------------
    
    
    
    
    output$myInd<- renderChart2({
      results_indR<-Data_indR > input$exp
      results_indC<-Data_indC > input$expC
      Fever_ind<-results_indR * results_indC
      
      IslandParts=c("East","High_land","South","Fringe","excepted_East","excepted_High_land","Total")
      if (input$Cluster_ind %in% IslandParts ) 
      { 
       Fever=Fever_ind[ ,input$Cluster_ind] 
       
      }
      
#       if (input$Cluster_ind=="East")                  { (Fever=Fever_ind[ ,East])}
#       if (input$Cluster_ind=="High_land")             { (Fever=Fever_ind[ ,High_land])}
#       if (input$Cluster_ind=="South")                 { (Fever=Fever_ind[ ,South])}
#       if (input$Cluster_ind=="Fringe")                { (Fever=Fever_ind[ ,Fringe])}
#       if (input$Cluster_ind=="excepted_East")         { (Fever=Fever_ind[ ,excepted_East])}
#       if (input$Cluster_ind=="excepted_High_land")    { (Fever=Fever_ind[ ,excepted_High_land])}
#       if (input$Cluster_ind=="Total")                 { (Fever=Fever_ind[ ,Total])}
      
      Fever<-as.xts(apply(Fever,1,sum, na.rm=TRUE))
      
      if (input$Cluster_ind=="East")                  { (Data_sentinel=exo_East)}
      if (input$Cluster_ind=="High_land")             { (Data_sentinel=exo_High_land)}
      if (input$Cluster_ind=="South")                 { (Data_sentinel=exo_South)}
      if (input$Cluster_ind=="Fringe")                { (Data_sentinel=exo_Fringe)}
      if (input$Cluster_ind=="excepted_East")         { (Data_sentinel=exo_No_East)}
      if (input$Cluster_ind=="excepted_High_land")    { (Data_sentinel=exo_No_Hgl)}
      if (input$Cluster_ind=="Total")                 { (Data_sentinel=exo_National)}
      
      
      results<-merge(Fever, Data_sentinel)
      results$ratio<-round((results$Fever/ results$sites_actifs)*100,0)
      results<-as.data.frame(results)     
      results<-tail(results,input$daterange_ind*52)
      axis_graph<-tail(ref_week,input$daterange_ind*52)
      
      h5 <- Highcharts$new()
      h5$series(data = results$caid, type="area",name = "IRS", enabled=F, visible=FALSE)
      h5$series(data = results$mild, type="area",name = "LLIN", visible=FALSE)
      h5$series(data = results$pmm, type="line", name = "Rainfall", visible=FALSE)
      h5$series(data = results$lst, type="line", name = "Temperature", visible=FALSE) 
      h5$series(data = results$ndvi,  type='line', name = "Ndvi", visible=FALSE)
      h5$series(data = results$ratio, type="line",name = "Alert",enabled=T,width=0.5)
      h5$colors('rgba(52,73,94, 1.0)', 'rgba(41,128,185,1.0)','rgba(122,175,255,.9)','rgba(255,117,71 ,.9)','rgba(22,160,133,1.0)','rgba(255,0,0,.9)')
      h5$yAxis( min = 0)
      h5$legend(symbolWidth = 30)
      h5$plotOptions(line=list(marker=list(enabled = F)),area=list(marker=list(enabled = F)))
      h5$chart(zoomType = "x1")
      h5$set(dom = "myInd")
      h5$exporting(enabled = F)
      h5$xAxis(type='datetime', categories=axis_graph, tickInterval = 30)
      return(h5)
      
    })
    
    
    # Level_plot --------------------------------------------------------------   
    
    
    
    output$table <- renderDataTable(testt())          
    output$temp <-renderPlot({
      
      colnames(Percentile)<-c("Ambovombe","Ambatondrazaka","Antsohihy","Anjozorobe","Antsirabe","Belo sur Tsiribihina",
                              "Behoririka","Ambato Boeny","Ambositra","Cd Andohatapenaka","Diego","Mandritsara","Edjeda",
                              "Farafangana","Fianarantsoa","Ihosy","Maevatanana","Morondava","Mahajanga","Miandrivazo",
                              "Manjakaray","Mananjary","Morombe","Moramanga","Maroantsetra","Maintirano","Nosy Be","Sambava",
                              "Sainte-Marie","Tsiroanomandidy","Tolagnaro","Toliary","Toamasina","Tsaralalana")
      
      if (input$facies=="East")                  { (results.cluster=Percentile[ ,Names_plot_East])}
      if (input$facies=="High_land")             { (results.cluster=Percentile[ ,Names_plot_High_land])}
      if (input$facies=="South")                 { (results.cluster=Percentile[ ,Names_plot_South])}
      if (input$facies=="Fringe")                { (results.cluster=Percentile[ ,Names_plot_Fringe])}
      if (input$facies=="excepted_East")         { (results.cluster=Percentile[ ,Names_plot_excepted_East])}
      if (input$facies=="excepted_High_land")    { (results.cluster=Percentile[ ,Names_excepted_High_land])}
      if (input$facies=="Total")                 { (results.cluster=Percentile[ ,Names_plot_total])}
      
      
      if (input$comet_P=="1"){
        datac<-results.cluster> input$Centile_comet}
      
      if (input$comet_P=="2"){
        datac<- lag(results.cluster> input$Centile_comet,0) & lag(results.cluster> input$Centile_comet,1)}
      
      if (input$comet_P=="3"){
        datac<- lag(results.cluster> input$Centile_comet,0) & lag(results.cluster> input$Centile_comet,1) & lag(results.cluster> input$Centile_comet,2)}
      
      datac<-as.data.frame(datac)
      row.names(datac)<-ref_week
      datac<-last(datac,input$daterange_comet*52)
      datac[datac=="TRUE"]<-1
      ref<-seq(1,length(datac[,1]),by=5)
      ref1<-seq(0,1,by=0.5)
      datac<-as.matrix(datac)
      
      level<-levelplot(datac, aspect=.6, 
                       at= ref1,col.regions = colorRampPalette(c("cadetblue3","red4")), colorkey=FALSE,
                       xlab='Date', ylab='Site',
                       scales=list(y=list(alternating=1), x=list(alternating=1, rot=90, format="%m_%Y", cex=1,at=ref)),
                       strip=strip.custom(par.strip.text=list(cex=1), bg=NA))
      print(level)
      
    })
    
    
    # Malaria_cases -----------------------------------------------------------
    
    
    
    # target_sites <- reactive({toString(input$sites)})
    
    
    output$malariacases <-renderChart2({
      
      
      graph1=site_new[, input$sites, drop = FALSE]
      graph1<-as.data.frame(graph1)
      colnames(graph1)<-c("sites")
      row.names(graph1)<-paste(c("week"),rownames(graph1))
      
      graph2=site_base[, input$sites, drop = FALSE]
      graph2<-as.data.frame(graph2)
      colnames(graph2)<-c("sites")
      row.names(graph2)<-paste(c("week"),rownames(graph2))
      
      graph3=site_mean[, input$sites, drop = FALSE]
      graph3<-as.data.frame(graph3)
      colnames(graph3)<-c("sites")
      row.names(graph3)<-paste(c("week"),rownames(graph3))
      
      graph4=percent[, input$sites, drop = FALSE]
      graph4<-as.data.frame(graph4)
      colnames(graph4)<-c("sites")
      row.names(graph4)<-paste(c("week"),rownames(graph4))
      
      
      ref_malaria<-row.names(graph3)
      
      
      h6 <- Highcharts$new()
      h6$series(data = graph4$sites, type="areaspline",
                name = "90th Percentile",enabled=T,width=0.5,fillOpacity = 0.3)
      h6$series(data = graph3$sites, type="line", name = " Weekly mean 2009-2014",fillOpacity = 0.8)
      h6$series(data = graph1$sites, type="column", name = "RDT+",fillOpacity = 0.3)
      h6$colors('rgba(192,192,192,.9)','rgba(30,144,255,0.4)','rgba(178,34,34, .9)')
      h6$legend(symbolWidth = 30)
      h6$yAxis( min = 0)
      h6$plotOptions(line=list(marker=list(enabled = F)),area=list(marker=list(enabled = F)))
      h6$chart(zoomType = "x1")
      h6$set(dom = "malariacases")
      h6$xAxis(type='datetime', categories=ref_malaria, tickInterval = 30)
      h6$exporting(enabled = F)
      return(h6)
      
    })
    
    
    # Eval_algorithms ---------------------------------------------------------
    
    
    
    # target_sites <- reactive({toString(input$sites)})
    
    
    output$eval_algorithms <-renderChart2({
      
      
      evaluate=Eval_algo[, input$sites_eval, drop = FALSE]
      evaluate<-as.data.frame(evaluate)
      colnames(evaluate)<-c("sites")
      
      h7 <- Highcharts$new()
      h7$series(data = evaluate$sites, type="line", name = "TDR +",fillOpacity = 0.8)
      h7$colors('rgba(204,0,0, 1.0)','rgba(178,34,34, .9)')
      h7$legend(symbolWidth = 30)
      h7$yAxis( min = 0)
      h7$plotOptions(line=list(marker=list(enabled = F)),area=list(marker=list(enabled = F)))
      h7$chart(zoomType = "x1")
      h7$set(dom = "eval_algorithms")
      h7$xAxis(type='datetime', categories=ref_week, tickInterval = 30)
      h7$exporting(enabled = F)
      return(h7)
      
    })
    
    
    
  })
})

