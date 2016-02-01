#required libraries
source("libraries.R")
#versatile app:
if ( getwd()!="/srv/shiny-server/sentinel_hrmntr") 
{
  setwd('/media/herimanitra/DONNEES/IPM_sentinelle/sentinel_hrmntr 291115/Sentinel')
} else {
 
}


##############################################server ######################
source("facies_class.R")
server<-function(input, output,session) {
  source("import1.r")
  #reactive computation of detection algorithms 
  #(centile,MinSan,C-sum,TDR+/fiever==>case of Malaria) 
  #depending on different parameters:
  preprocessing = reactive({
    ##############selection of disease data depending on user input choice##
    source("diseases_control.R")
    mydata=select_disease(disease=input$diseases)
    ####################################data preprocessing#############
    cat("convert PaluConf table to data.table format...")
    mydata=as.data.table(as.data.frame(mydata))
    cat('DONE\n')
    cat("keep only sites that already have historical values...\n")
    mydata=mydata[,include,with=F]
    cat('reshape PaluConf...')
    mydata=as.data.table(gather(mydata,key=sites,value=occurence,-c(code,deb_sem)))
    cat('DONE\n')
    
    source("create_facies.R")
    mydata=create_facies(mydata)
    
    cat('merge with different facies...')
    setkey(mydata,sites)
    mydata[sites %in% East,East:=1]
    mydata[sites %in% South,South:=1]
    mydata[sites %in% High_land,High_land:=1]
    mydata[sites %in% Fringe,Fringe:=1]
    mydata[sites %in% excepted_East,excepted_East:=1]
    mydata[sites %in% excepted_High_land,excepted_High_land:=1]
    cat('DONE\n')
    
    cat('Extract weeks and years from PaluConf...')
    mydata[,weeks:=as.numeric(substr(code,6,8))]
    mydata[,years:=as.numeric(substr(code,1,4))]
    cat('DONE\n')
    cat("convert site to character...")
    mydata[,sites:=as.character(sites)]
    setkeyv(mydata,c('sites','weeks','years','code'))
    cat("DONE\n")
    
    return(mydata)
  })
  percentile_algorithm = reactive({
    mydata=preprocessing()
    cat('Calculation of percentile and proportion of sites in alert begin...\n')
    source("percentile.R")
    mylist= calculate_percentile(data=mydata,
                                 #week_choice=input$week_choice,
                                 # year_choice=input$year_choice
                                 week_length=input$comet_map,
                                 percentile_value=input$Centile_map
                                )
    return(mylist)
  })
  minsan_algorithm = reactive({
    mydata=preprocessing()
    #####################################Doublement du nb de cas (MinSan algo) ###################
    cat('Calculation of minsan and proportion of sites in alert begin...\n')
    source("minsan.R")
    mylist=calculate_minsan(data=mydata,slope_minsan=input$slope_minsan,
                            #week_choice=input$week_choice,
                            #year_choice=input$year_choice,
                            minsan_weekrange=input$minsan_weekrange,
                            minsan_consecutive_week=input$minsan_consecutive_week,
                            byvar="code")
    return (mylist)
  })
  csum_algorithm = reactive({
    mydata=preprocessing()
    ######################################Cumulative sum (cumsum algo) ##########################
    cat('Calculation of csum and proportion of sites in alert begin...')
    source("csum.R")
    cat('DONE\n')
    mylist= calculate_csum(data=mydata,
                           Csum_year_map=input$Csum_year_map,
                           Csum_week_map=input$Csum_week_map,
                           Sd_csum_map=input$Sd_csum_map,
                           #week_choice=input$week_choice,
                           week_Csum_map=input$week_Csum_map
                          # year_choice=input$year_choice
                           ,byvar="code")
    return (mylist)
    ##############################Output all results in a list: ##################################
    
  })
  tdrplus_fever_ind = reactive({
    ##############################TDR + / Fever Indicator algorithm###################
    cat('convert Consultations into data.table format...')
    Consultations=as.data.table(as.data.frame(Consultations))
    Consultations=Consultations[,include,with=F]
    cat('DONE\n')
    cat("convert PaluConf table to data.table format...")
    PaluConf=as.data.table(as.data.frame(PaluConf))
    PaluConf=PaluConf[,include,with=F]
    
    cat('DONE\n')
    cat("convert SyndF table to data.table format...")
    SyndF=as.data.table(as.data.frame(SyndF))
    SyndF=SyndF[,include,with=F]
    cat('DONE\n')
    cat('reshape Consultations...')
    Consultations=as.data.table(gather(Consultations,
                                  key=sites,
                                  value=nb_consultation,-c(code,deb_sem)))
    cat('DONE\n')
    cat('reshape PaluConf...')
    PaluConf=as.data.table(gather(PaluConf,
                                  key=sites,
                                  value=malaria_cases,-c(code,deb_sem)))
    cat('DONE\n')
    cat('reshape SyndF...')
    SyndF=as.data.table(gather(SyndF,
                                 key=sites,
                                 value=nb_fievre,-c(code,deb_sem)))
    cat('DONE\n')
    cat("merge PaluConf and SyndF...")
    PaluConf_SyndF=merge(PaluConf,SyndF[,list(sites,deb_sem,nb_fievre)],by.x=c("sites","deb_sem")
                        ,by.y=c("sites","deb_sem"))
    cat('DONE\n')
   
    cat("merge PaluConf_SyndF and Consultations...")
    PaluConf_SyndF=merge(PaluConf_SyndF,Consultations[,list(sites,deb_sem,nb_consultation)],by.x=c("sites","deb_sem")
                        ,by.y=c("sites","deb_sem"))
   cat('DONE\n')
   
   cat('Extract weeks and years from PaluConf...')
   PaluConf_SyndF[,weeks:=as.numeric(substr(code,6,8))]
   PaluConf_SyndF[,years:=as.numeric(substr(code,1,4))]
   cat('DONE\n')
   cat("convert site to character...")
   PaluConf_SyndF[,sites:=as.character(sites)]
   cat("DONE\n")
   
   cat('looking for an alert when Malaria cases among fever cases exceed:',
       input$exp_map,'% or malaria cases among consultation number...',
       input$expC_map,'...')
   PaluConf_SyndF[,alert_status:=ifelse(100*sum(malaria_cases,na.rm = T)/sum(nb_fievre,na.rm = T)>as.numeric(input$exp_map) &
                                         100*sum(malaria_cases,na.rm = T)/sum(nb_consultation,na.rm = T)>as.numeric(input$expC_map),
                                        "alert","normal"),by="sites"] 
   PaluConf_SyndF[,alert_status_hist:=ifelse(100*(malaria_cases)/(nb_fievre)>as.numeric(input$exp_map) &
                                          100*(malaria_cases)/(nb_consultation)>as.numeric(input$expC_map),
                                        "alert","normal")]
   cat('DONE\n')
   ###proportion of sites in alert (weekly for all, and weekly by facies)##########
   cat('calculate weekly proportion of sites in alert using tdr+/fever algorithm...\n')
   
   Nbsite_beyond=PaluConf_SyndF[ alert_status_hist=="alert",
                      length(unique(sites)),by="code"]
   setnames(Nbsite_beyond,"V1","eff_beyond")
   Nbsite_beyond=merge(Nbsite_beyond,unique(PaluConf_SyndF[,list(code,deb_sem)]),
                       by.x="code",by.y="code") #,all.x=T
   Nbsite_withdata=PaluConf_SyndF[is.na(alert_status_hist)==F,length(unique(sites)),by="code"]
   setnames(Nbsite_withdata,"V1","eff_total")
   propsite_alerte_fever=merge(x=Nbsite_withdata,y=Nbsite_beyond,
                                by.x="code",by.y="code")
   propsite_alerte_fever[,prop:=ifelse(is.na(eff_beyond/eff_total)==T,0.0,
                                        eff_beyond/eff_total)]
   cat('DONE\n')
   
   cat('calculate weekly proportion of sites in alert using tdr+/fever algorithm (by facies)...\n')
   source("create_facies.R")
   PaluConf_SyndF=create_facies(data=PaluConf_SyndF)
   Nbsite_beyond=PaluConf_SyndF[ alert_status_hist=="alert",
                                 length(unique(sites)),by=c("code","facies")]
   setnames(Nbsite_beyond,"V1","eff_beyond")
   Nbsite_beyond=merge(Nbsite_beyond,unique(PaluConf_SyndF[,list(code,deb_sem,facies)]),
                       by.x=c("code","facies"),by.y=c("code","facies"))
   Nbsite_withdata=PaluConf_SyndF[is.na(alert_status_hist)==F,
                                  length(unique(sites)),by=c("code","facies")]
   setnames(Nbsite_withdata,"V1","eff_total")
   propsite_alerte_fever_byfacies=merge(x=Nbsite_withdata,y=Nbsite_beyond,
                               by.x=c("code","facies"),by.y=c("code","facies") )
   propsite_alerte_fever_byfacies[,prop:=ifelse(is.na(eff_beyond/eff_total)==T,0.0,
                                       eff_beyond/eff_total)]
   rm(Nbsite_beyond);rm(Nbsite_withdata);gc()
   cat('DONE\n')
  
   cat('calculate radius for per site for percentile algorithm alert...')
   PaluConf_SyndF[,nbsite_alerte:=1.0]; PaluConf_SyndF[,nbsite_normal:=1.0]
   setkey(PaluConf_SyndF,alert_status_hist)
   PaluConf_SyndF[alert_status_hist=="alert",nbsite_alerte:=sum(malaria_cases,na.rm = T)*1.0,by="sites,code"]
   PaluConf_SyndF[alert_status_hist=="normal",nbsite_normal:=sum(malaria_cases,na.rm = T)*1.0,by="sites,code"]
   PaluConf_SyndF[alert_status_hist=="normal",myradius:=5*(nbsite_alerte+1)/sqrt(nbsite_normal+1)]
   PaluConf_SyndF[alert_status_hist=="alert",myradius:=(nbsite_alerte+1)/(nbsite_normal+1)]
   PaluConf_SyndF[alert_status_hist %in% NA, myradius:=10*myradius]
   cat('DONE\n')
   
   return(list(
               tdrplus_ind_currentweek = PaluConf_SyndF[as.Date(deb_sem)==max(as.Date(deb_sem))-7,],
               propsite_alerte_fever=propsite_alerte_fever,
               propsite_alerte_fever_byfacies=propsite_alerte_fever_byfacies))
  })
  #Malaria cases and weekly mean:
  output$malariacases <-renderPlotly({
    cat('retrieving code corresponding to selected sites...')
    setkey(sentinel_latlong,name)
    mysite=sentinel_latlong[name %in% grep(input$mysites,name,value=T),get("sites") ]
    cat('==>',mysite,'...')
    cat('DONE\n')
    mydata=preprocessing() 
    cat('fetching number of cases per week and sites for visualization...')
    setkey(mydata,years)
    graph1=mydata[years==2015,sum(occurence,na.rm = T),by="weeks,sites"]
    #rename some cols and rows:
    setnames(graph1,old="V1",new="cases")
    cat('DONE\n')
    cat('fetching mean cases per week and sites for visualization...')
    graph2=mydata[years %in% 2009:2014,mean(occurence,na.rm = T),by="weeks,sites"]
    setnames(graph2,old="V1",new="weeklymean")
    cat('DONE\n')
    cat("fetching the",input$Centile_map,"th percentile for all years...")
    graph3=mydata[,quantile(occurence,na.rm = T,probs = input$Centile_map/100),by="sites"]
    setnames(graph3,old="V1",new="percentile90")
    cat('DONE\n')
    cat("merging number of cases,mean cases and",input$Centile_map,"th percentile...")
    mygraph=merge(graph1,graph2,by.x=c("sites","weeks"),by.y=c("sites","weeks"))
    mygraph=merge(mygraph,graph3,by.x="sites",by.y="sites")
    rm(graph3);rm(graph2);rm(graph1);gc()
    cat('DONE\n')
    
    #additionnal transformation to produce stacked bar chart:
    mygraph=mygraph[sites==mysite,]
    mygraph[,value1:=ifelse(cases>percentile90,cases-percentile90,0)]
    mygraph[,value2:=ifelse(cases>percentile90,percentile90,cases)]
    
    tmp1=mygraph[,list(weeks,value1)];setnames(tmp1,"value1","cases")
    tmp2=mygraph[,list(weeks,value2)];setnames(tmp2,"value2","cases")
    tmp1[,Status:=">threshold"]
    tmp2[,Status:="Normal"]
    mygraph=rbind(tmp1,tmp2)
    cols=c("Normal"="blue",">threshold"="red")
    setkeyv(mygraph,c("weeks","cases"))
    #end 
    h <- ggplot(mygraph, aes(x=weeks, y=cases,fill=Status)) +
      geom_bar(stat="identity") + scale_fill_manual(values=cols) + ggtitle(paste("Malaria cases in",input$mysites,"since 01/01/2015")) 
    ggplotly(h)
  })
  #display proportion of sites in alert
  output$propsite_alerte = renderDygraph ({
    mytitle=paste0("Weekly prop. of sites in alert using ",
                   input$Algorithmes_eval,
                   " and High Frequency indicators")
    
    source("create_facies.R"); 
    #loading and transforming HF Indicators:
    source("introducing_caid.R",local = T)
    source("introducing_mild.R",local = T)
    source("introducing_pmm.R",local = T)
    source("introducing_lst.R",local = T) 
    source("introducing_ndvi.R",local = T)
    #append HFI depending on user choices:
    source("if_percentile_viz.R",local = T)
    source("if_minsan_viz.R",local = T)
    source("if_csum_viz.R",local = T)
    source("if_tdrfiever_viz.R",local = T)
    ################################################################### 
    cat('nrow of myprop before merging are:',nrow(myprop),'\n')
    if ( input$Cluster_algo !="Total"  )
    {
      cat('merging ndvi data with proportion of sites in alert...')
      myprop=merge(myprop,ndvi[,list(code,ndvi_value,facies)],
                   by.x=c("code","facies"),
                   by.y=c("code","facies"))
      cat('DONE\n')
      cat('nrow of myprop  after merge with ndvi are:',nrow(myprop),"\n")
      
      cat('merging temperature data with proportion of sites in alert...')
      myprop=merge(myprop,lst[,list(code,temperature,facies)],
                   by.x=c("code","facies"),
                   by.y=c("code","facies"))
      cat('DONE\n')
      
      cat('nrow of myprop  after merge with lst are:',nrow(myprop),"\n")
      
      cat('merging rainFall data with proportion of sites in alert...')
      myprop=merge(myprop,pmm[,list(code,pmm_value,facies)],by.x=c("code","facies"),
                   by.y=c("code","facies"))
      cat('DONE\n')
      
    } else {
      cat('merging ndvi data with proportion of sites in alert...')
      myprop=merge(myprop,ndvi[,list(code,ndvi_value)],
                   by.x=c("code"),
                   by.y=c("code") )
      cat('DONE\n')
      cat('nrow of myprop  after merge with ndvi are:',nrow(myprop),"\n")
      
      cat('merging temperature data with proportion of sites in alert...')
      myprop=merge(myprop,lst[,list(code,temperature)],
                   by.x=c("code"),
                   by.y=c("code") )
      cat('DONE\n')
      
      cat('nrow of myprop  after merge with lst are:',nrow(myprop),"\n")
      
      cat('merging rainFall data with proportion of sites in alert...')
      myprop=merge(myprop,pmm[,list(code,pmm_value)],
                   by.x=c("code"),
                   by.y=c("code") )
      cat('DONE\n')
      
      
    }
   ################################################################### 
   if (input$Cluster_algo =="Total" )
      {
        cat('merging LLIN data with proportion of sites in alert...')
        myprop=merge(myprop,mild[,list(code,mild_value)],
                     by.x=c("code"),
                     by.y=c("code"),all.x=T)
        cat('DONE\n')
        cat('nrow of myprop  after merge with llin are:',nrow(myprop),"\n")
        
        cat('merging CAID/IRS data with proportion of sites in alert...')
        myprop=merge(myprop,caid[,list(code,caid_value)],by.x=c("code"),
                     by.y=c("code"),all.x=T)
        cat('DONE\n')
        cat('nrow of myprop  after merge with caid are:',nrow(myprop),"\n")
        
      } else {
        cat('merging LLIN data with proportion of sites in alert...')
        myprop=merge(myprop,mild[,list(code,mild_value,facies)],
                     by.x=c("code","facies"),
                     by.y=c("code","facies"))
        cat('DONE\n')
        cat('nrow of myprop  after merge with llin are:',nrow(myprop),"\n")
        
        cat('merging CAID/IRS data with proportion of sites in alert...')
        myprop=merge(myprop,caid[,list(code,caid_value,facies)],
                     by.x=c("code","facies"),
                     by.y=c("code","facies"))
        cat('DONE\n')
        cat('nrow of myprop  after merge with caid are:',nrow(myprop),"\n")
      }
      ####################################################################
      cat('cbind time series data for visualization...')
      semaine= as.Date(myprop$deb_sem )  
      myprop0=cbind(prop=xts(100*myprop$prop,order.by=semaine))
      #remove UTC date:
#       old_name=names(myprop0)
#       myprop0= to.weekly(myprop0)
#       myprop0 = myprop0[,1]
#       names(myprop0)=old_name
      #
      cat('DONE\n')
      #Initialization of the data:
      a= dygraph( data= myprop0  ,main =  mytitle)  
      a= a %>% dySeries("prop", label = "%sites in alert") 
      
      cat("display High Frequency Indicators depending on check box choices...\n")
      source("temp_choice.R",local = T)
      source("llin_choice.R",local = T)
      source("irs_choice.R",local = T)
      source("ndvi_choice.R",local = T)
      source("pmm_choice.R",local = T)
      cat("DONE\n")
      #final output:
      a= a %>%   dyAxis("y", label = "Values") %>% dyRangeSelector() 
      return(a)

  })
  #display sites in alert for the current week into the map
  output$madagascar_map <- renderLeaflet({
    if (input$Algorithmes_eval=="Percentile") 
    {
      cat("display alert status into the map using percentile algorithm...\n")
      #setkey for fast merging
      setkey(sentinel_latlong,sites)
      setkey(percentile_algorithm()$percentile_alerte_currentweek,sites)
      sentinel_latlong=merge(sentinel_latlong,percentile_algorithm()$percentile_alerte_currentweek
                             ,by.x="sites",by.y = "sites")
      
    }
    if (input$Algorithmes_eval=="MinSan") 
    {
      cat("display alert status into the map using MinSan algorithm...\n")
      setkey(sentinel_latlong,sites);
      setkey(minsan_algorithm()$minsan_alerte_currentweek,sites)
      sentinel_latlong=merge(sentinel_latlong,minsan_algorithm()$minsan_alerte_currentweek
                             ,by.x="sites",by.y = "sites")
    }
    if (input$Algorithmes_eval=="Csum") 
    {
      cat("display alert status into the map using Csum algorithm...\n")
      setkey(sentinel_latlong,sites);
      setkey(csum_algorithm()$csum_alerte,sites)
      sentinel_latlong=merge(sentinel_latlong,csum_algorithm()$csum_alerte_currentweek
                             ,by.x="sites",by.y = "sites")
    }
    if (input$Algorithmes_eval=="Ind") 
    {
      setkey(sentinel_latlong,sites);
      setkey(tdrplus_fever_ind()$tdrplus_ind_currentweek,sites)
      sentinel_latlong=merge(sentinel_latlong,tdrplus_fever_ind()$tdrplus_ind_currentweek
                            ,by.x="sites",by.y = "sites")
      cat("display alert status into the map using a simple indicator...\n")
    }
    
    #feeding leaflet with our data:
    madagascar_map=leaflet()
    madagascar_map=leaflet(sentinel_latlong) 
    madagascar_map=madagascar_map %>% setView(lng = 47.051532 , 
                                              lat =-19.503781 , zoom = 6) 
    madagascar_map=madagascar_map %>% addTiles() 
    #change color to red when alert is triggered:
    #navy
    pal <- colorFactor(c("red", "green"), domain = c("normal", "alert"))
    madagascar_map=madagascar_map %>% addCircleMarkers(lng = ~Long, 
                                                       lat = ~Lat, 
                                                       weight = 1,
                                                       radius = ~myradius, 
                                                       color = ~pal(alert_status),
                                                       popup = ~name)
  })
  #click event handler on leaflet map:
  selected_site=eventReactive(input$madagascar_map_marker_click,{
      event <- input$madagascar_map_marker_click
      return(sentinel_latlong[Long==event$lng & Lat==event$lat,get("sites")])
    })
  selected_sitename=eventReactive(input$madagascar_map_marker_click,{
    event <- input$madagascar_map_marker_click
    return(sentinel_latlong[Long==event$lng & Lat==event$lat,get("name")])
  })
  selected_site_ontheleft = eventReactive(input$mysites, {
    #if the user has click on the params on the left of the map:
    event = input$mysites
    if ( event=="" )
    {
      return (NULL)
    } else {
      #search the name to match with sentinel_latlong:
      return(sentinel_latlong[name %in% grep(event,name,value=T),get("sites") ])
    }
    
  })
  selected_sitename_ontheleft = eventReactive(input$mysites, {
    #if the user has click on the params on the left of the map:
    event = input$mysites
    if ( event=="" )
    {
      return (NULL)
    } else {
      #search the name to match with sentinel_latlong:
      return(sentinel_latlong[name %in% grep(event,name,value=T),get("name") ])
    }
  })
  #render weekly malaria cases for a clicked site
  output$weekly_disease_cases_persite = renderPlotly({
    mydata=preprocessing()
    setkey(mydata,sites)
    sites_list=c(selected_site(),selected_site_ontheleft())
    #if the user has clicked on the map:
    mydata=mydata[sites %in% sites_list ,
                    list(occurence=sum(occurence,na.rm=T)),by="deb_sem"]
    #reorder time series
    mydata[,deb_sem:=as.Date(deb_sem)]
    mydata=mydata[order(deb_sem),]
    setnames(mydata,"deb_sem","Semaine")
    #
  if (is.null(selected_sitename_ontheleft()) )
    {
    mytitle=paste("Historical", 
                  input$diseases ,"cases in",selected_sitename() )
  } else {
    mytitle=paste("Historical", 
                  input$diseases ,"cases in",selected_sitename()," and",
                  selected_sitename_ontheleft() )
    }
    #
    p=plot_ly(data=mydata,
              y=occurence,
              x=Semaine)
    p %>% layout(title=mytitle)
    
  })
  #Health heatmap plot with plotly and using percentile algorithm:
  output$heatmap_percentile = renderD3heatmap({
    mydata=preprocessing()
    cat('Calculation of percentile and proportion of sites in alert begin...\n')
    source("percentile.R")
    propsite_alerte_percentile=calculate_percentile(data=mydata,
                                                   # week_choice=input$week_choice,
                                                   # year_choice=input$year_choice
                                                    week_length=input$comet_map,
                                                    percentile_value=input$Centile_map)$propsite_alerte_percentile
                                                   
   #selection by facies:
    if (input$Cluster_algo !="Total")
    {
      cat("you choose:",input$Cluster_algo,"\n")
      setkeyv(propsite_alerte_percentile,input$Cluster_algo)
      propsite_alerte_percentile=propsite_alerte_percentile[get(input$Cluster_algo)==1,]
    }
    
    cat('recode alert status...')
    setkey(propsite_alerte_percentile,alert_status)
    propsite_alerte_percentile[is.na(alert_status)==T,alert_status2:=0]
    propsite_alerte_percentile[alert_status=="normal",alert_status2:=1]
    propsite_alerte_percentile[alert_status=="alert",alert_status2:=2]
    cat('DONE\n')
    
    
    cat('spreading data...')
    #try selection using dplyr so then avoid data.frame conversion!
    myz= as.data.frame(spread(unique(propsite_alerte_percentile[,list(sites,deb_sem,alert_status2)]),
                              deb_sem,alert_status2))
    cat("DONE\n")
    row.names(myz) <- myz$sites
    d3heatmap(myz[,-1], dendrogram = "none",scale = "none",
              xaxis_font_size="9px",
              color=c("grey","blue","red"))


    
    
  })
  #Bubble chart to display past alert:
  output$mybubble = renderPlotly({
    mydata=preprocessing()
    cat('Calculation of percentile and proportion of sites in alert begin...\n')
    source("percentile.R")
    X=calculate_percentile(data=mydata,
                          # week_choice=input$week_choice,
                          #year_choice=input$year_choice
                           week_length=input$comet_map,
                           percentile_value=input$Centile_map)$mydata
                           
    if (input$Cluster_algo=="Total")
    {
      X=X[alert_status=="alert" ,]
    } else {
      X=X[alert_status=="alert" & get(input$Cluster_algo)==1,]
    }
    X=merge(X,sentinel_latlong,by.x="sites",by.y="sites")
    setorder(X,sites,deb_sem)
    plot_ly(X, y = log(occurence+1), x=deb_sem,
            color=name, 
            size = log(occurence+2), mode = "markers")
    #type="scatter3d"
  
  })
  #download report handler (for Malaria and Diarrhée):
  output$downloadReport <- downloadHandler(
    #source("generate_report.R")
    filename = "report.pdf",
    content = function(file) {
      file.copy('report.pdf', file)
    }
  )
}

##############################################User interface ##############
#skeleton of the user interface:
source('initialize_ui.r')
ui = dashboardPage(skin = "blue",
  dashboardHeader(title="Surveillance sentinelle",titleWidth="233"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(text="Main",tabName="mytabbox", 
               icon = icon("database")),
      diseases_choices,
      myfacies_algo,
      
      menuItem(text="Download diseases report",
               tabName="diparam", 
               icon = icon("building")),
      menuItem(text="Forecasting",
               tabName="myforecast", 
               icon = icon("line-chart"))

  )),
  dashboardBody(tabItems(tabbox_item,
                         disease_item,
                         forecast_item
                         ))
)
#assemble UI and SERVER:
shinyApp(ui, server)








