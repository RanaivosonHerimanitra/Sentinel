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
  source("import_data.R")
  #reactive computation of detection algorithms 
  #(centile,MinSan,C-sum,TDR+/fiever==>case of Malaria) 
  #depending on different parameters:
  preprocessing = reactive({
    ##############selection of disease data depending on user input choice##
    source("diseases_control.R")
    mydata=select_disease(disease=input$diseases)
    ####################################data preprocessing############
    cat("keep only sites that already have historical values...\n")
    include_index= match(include,names(mydata)) #introduce dplyr
    #mydata=mydata[,include,with=F]
    mydata= mydata %>% select(include_index) %>% as.data.frame()
    
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
                           week_Csum_map=input$week_Csum_map
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
     setkey(PaluConf_SyndF,alert_status_hist)
     PaluConf_SyndF[alert_status=="alert", myradius:=15.0]
     PaluConf_SyndF[alert_status_hist=="normal", sum_occurence_week:=sum(malaria_cases,na.rm=T),by="code"]
     PaluConf_SyndF[alert_status=="normal", myradius:=15.0*malaria_cases/sum_occurence_week,by="sites,code"]
     PaluConf_SyndF[alert_status_hist %in% NA, myradius:=5.0]
   cat('DONE\n')
   if (max(as.Date(PaluConf_SyndF$deb_sem)) ==Sys.Date() )
   {
     tdrplus_ind_currentweek = PaluConf_SyndF[as.Date(deb_sem)==max(as.Date(deb_sem))-7,]
   } else {
     tdrplus_ind_currentweek = PaluConf_SyndF[as.Date(deb_sem)==max(as.Date(deb_sem)),]
   }
   return(list(mydata=PaluConf_SyndF,
               tdrplus_ind_currentweek =tdrplus_ind_currentweek,
               propsite_alerte_fever=propsite_alerte_fever,
               propsite_alerte_fever_byfacies=propsite_alerte_fever_byfacies))
  })
  
  #display proportion of sites in alert with these HFI
  output$propsite_alerte = renderPlotly({
    source("create_facies.R")
    cat("loading and transforming HF Indicators...")
     hfi=data.table(gather(hfi,key=sites,
                          value=myvalue,-c(code,deb_sem,type_val)))
     hfi= create_facies(hfi)
    cat("DONE\n")
    cat("dispatch data from HFI...")
     setkey(hfi,type_val)
     caid = hfi[type_val=="CAID"];setnames(caid,old="myvalue",new="caid_value")
     pmm = hfi[type_val=="PRECIPITATION"];setnames(pmm,old="myvalue",new="pmm_value")
     lst = hfi[type_val=="LST_DAY"];setnames(lst,old="myvalue",new="temperature")
     ndvi = hfi[type_val=="NDVI"];setnames(ndvi,old="myvalue",new="ndvi_value")
    cat("DONE\n")
    source("introducing_caid.R",local = T) #==>now in caid
    source("introducing_mild.R",local = T)
    source("introducing_pmm.R",local = T) #==>now in hfi
    source("introducing_lst.R",local = T) #==>now in hfi
    source("introducing_ndvi.R",local = T) #==>now in hfi
   
    
    source("if_percentile_viz.R",local = T)
    source("if_minsan_viz.R",local = T)
    source("if_csum_viz.R",local = T)
    source("if_tdrfiever_viz.R",local = T)  
    
   
   
    
        if ( input$Cluster_algo !="Total"  )
        {
          setkeyv( mild , c("code","facies") )
          cat('merging mild data with proportion of sites in alert...')
           myprop=merge(myprop,unique(mild[,list(code,mild_value,facies)]),
                       by.x=c("code","facies"),
                       by.y=c("code","facies"), all.x=T)
          cat('DONE\n')
          cat('nrow of myprop  after merge with MILD are:',nrow(myprop),"\n")
          
          
          setkeyv( caid , c("code","facies") )
          cat('merging caid data with proportion of sites in alert...')
           myprop=merge(myprop,(caid[,list(code,caid_value,facies)]),
                       by.x=c("code","facies"),
                       by.y=c("code","facies"), all.x=T)
          cat('DONE\n')
          
          cat('nrow of myprop  after merge with CAID are:',nrow(myprop),"\n")
           
          setkeyv( myprop , c("code","facies") );setkeyv( ndvi , c("code","facies") )
           cat('merging ndvi data with proportion of sites in alert...')
            myprop=merge(myprop,(ndvi[,list(code,ndvi_value,facies)]),
                       by.x=c("code","facies"),
                       by.y=c("code","facies"), all.x=T)
           cat('DONE\n')
          
          cat('nrow of myprop  after merge with ndvi are:',nrow(myprop),"\n")
          
          cat('merging temperature data with proportion of sites in alert...')
           setkeyv( lst , c("code","facies") )
           myprop=merge(myprop,(lst[,list(code,temperature,facies)]),
                       by.x=c("code","facies"),
                       by.y=c("code","facies"), all.x=T)
          cat('DONE\n')
          
          cat('nrow of myprop  after merge with lst are:',nrow(myprop),"\n")
          
          cat('merging rainFall data with proportion of sites in alert...')
           setkeyv( pmm , c("code","facies") )
           myprop=merge(myprop,(pmm[,list(code,pmm_value,facies)]),
                       by.x=c("code","facies"),
                       by.y=c("code","facies"), all.x=T)
          cat('DONE\n')
          
          cat("selection of faciès...")
           myprop=myprop[facies==input$Cluster_algo]
          cat("DONE\n")
        } else {
          setkey(myprop,"code");setkey(caid,"code")
          cat('merging caid data with proportion of sites in alert...')
          myprop=merge(myprop,caid[,list(code,caid_value)],
                       by.x=c("code"),
                       by.y=c("code"), all.x=T )
          cat('DONE\n')
          setkey(myprop,"code");setkey(mild,"code")
          cat('merging mild data with proportion of sites in alert...')
          myprop=merge(myprop,mild[,list(code,mild_value)],
                       by.x=c("code"),
                       by.y=c("code"), all.x=T )
          cat('DONE\n')
          cat('merging ndvi data with proportion of sites in alert...')
          setkey(myprop,"code");setkey(ndvi,"code")
          myprop=merge(myprop,ndvi[,list(code,ndvi_value)],
                       by.x=c("code"),
                       by.y=c("code"), all.x=T )
          cat('DONE\n')
          cat('nrow of myprop  after merge with ndvi are:',nrow(myprop),"\n")
          cat('merging temperature data with proportion of sites in alert...')
          setkey(myprop,"code");setkey(lst,"code")
          myprop=merge(myprop,lst[,list(code,temperature)],
                       by.x=c("code"),
                       by.y=c("code"), all.x=T )
          cat('DONE\n')
          
          cat('nrow of myprop  after merge with lst are:',nrow(myprop),"\n")
          setkey(myprop,"code");setkey(pmm,code)
          cat('merging rainFall data with proportion of sites in alert...')
          myprop=merge(myprop,pmm[,list(code,pmm_value)],
                       by.x=c("code"),
                       by.y=c("code"), all.x=T )
          cat('DONE\n')
        }
   
    
    #using plotly:
    #line width (epaisseur de la ligne):
   
    line_width=1.0
    p <- plot_ly(myprop, x = deb_sem,
                 y = 100*prop,name=input$diseases,
                 line = list(width=line_width,color = "rgb(255, 0, 0)") )
    p = p %>% layout(title="%sites in alert")
    #these are only make sense when Malaria (not for other diseases)
    if ( input$diseases=="Malaria")
    {
      p = p %>% add_trace(x = deb_sem, y = caid_value, name = "IRS",
                          type="bar",
                          color= "IRS",
                          colors="#dfb678",
                          visible='legendonly')
      p = p %>% add_trace(x = deb_sem, y = mild_value, name = "LLIN",
                          color="Mild",
                          opacity=0.5,
                          colors="#132B43",
                          type="bar",
                          visible='legendonly')
    }
   
    p = p %>% add_trace(x = deb_sem, y = 100*ndvi_value, name = "NDVI",
                        line = list(width=line_width),visible='legendonly')
    p = p %>% add_trace(x = deb_sem, y = pmm_value, name = "Rainfall(mm)",
                        line = list(width=line_width,color="#307ff0"),
                        visible='legendonly')
    
    p = p %>% add_trace(x = deb_sem, y = temperature, name = "Temp.",
                        line = list(width=line_width,color = "#ff8d00"),
                        visible='legendonly')
    p = p %>% layout(legend = list(x = 0, y = 100),
                     xaxis =list(title="Weeks"),
                     yaxis =list(title="Values")
                     )

    p
    
  
  })
  #display sites in alert for the current week into the map (02 map choices currently)
  output$madagascar_map <- renderLeaflet({
    if (input$diseases=="Malaria")
    {
      if (input$Algorithmes_eval1=="Percentile" ) 
      {
        
        cat("display alert status into the map using percentile algorithm...\n")
        #setkey for fast merging
        setkey(sentinel_latlong,sites)
        setkey(percentile_algorithm()$percentile_alerte_currentweek,sites)
        sentinel_latlong=merge(sentinel_latlong,percentile_algorithm()$percentile_alerte_currentweek
                               ,by.x="sites",by.y = "sites")
        
      }
      if (input$Algorithmes_eval1=="MinSan"  ) 
      {
       
        cat("display alert status into the map using MinSan algorithm...\n")
        setkey(sentinel_latlong,sites);
        setkey(minsan_algorithm()$minsan_alerte_currentweek,sites)
        sentinel_latlong=merge(sentinel_latlong,minsan_algorithm()$minsan_alerte_currentweek
                               ,by.x="sites",by.y = "sites")
      }
      if (input$Algorithmes_eval1=="Csum"  ) 
      {
        cat("display alert status into the map using Csum algorithm...\n")
        setkey(sentinel_latlong,sites);
        setkey(csum_algorithm()$csum_alerte,sites)
        sentinel_latlong=merge(sentinel_latlong,csum_algorithm()$csum_alerte_currentweek
                               ,by.x="sites",by.y = "sites")
      }
      if (input$Algorithmes_eval1=="Ind" ) 
      {
        setkey(sentinel_latlong,sites);
        setkey(tdrplus_fever_ind()$tdrplus_ind_currentweek,sites)
        sentinel_latlong=merge(sentinel_latlong,tdrplus_fever_ind()$tdrplus_ind_currentweek
                               ,by.x="sites",by.y = "sites")
        cat("display alert status into the map using a simple indicator...\n")
      }
    } else {
      if (input$Algorithmes_eval2=="Percentile" ) 
      {
        cat("display alert status into the map using percentile algorithm...\n")
        #setkey for fast merging
        setkey(sentinel_latlong,sites)
        setkey(percentile_algorithm()$percentile_alerte_currentweek,sites)
        sentinel_latlong=merge(sentinel_latlong,percentile_algorithm()$percentile_alerte_currentweek
                               ,by.x="sites",by.y = "sites")
        
      }
      if (input$Algorithmes_eval2=="MinSan"  ) 
      {
        
        cat("display alert status into the map using MinSan algorithm...\n")
        setkey(sentinel_latlong,sites);
        setkey(minsan_algorithm()$minsan_alerte_currentweek,sites)
        sentinel_latlong=merge(sentinel_latlong,minsan_algorithm()$minsan_alerte_currentweek
                               ,by.x="sites",by.y = "sites")
      }
      if (input$Algorithmes_eval2=="Csum"  ) 
      {
        cat("display alert status into the map using Csum algorithm...\n")
        setkey(sentinel_latlong,sites);
        setkey(csum_algorithm()$csum_alerte,sites)
        sentinel_latlong=merge(sentinel_latlong,csum_algorithm()$csum_alerte_currentweek
                               ,by.x="sites",by.y = "sites")
      }
     
    }
      #exclude Haute Terre Centrale (High_land) from the map
    sentinel_latlong=sentinel_latlong[!(sites%in% High_land)]
    #DONE
    #feeding leaflet with our data:
    mada_map=leaflet()
    mada_map=leaflet(sentinel_latlong) 
    mada_map=mada_map %>% setView(lng = 47.051532 , 
                                              lat =-19.503781 , zoom = 5) 
    mada_map=mada_map %>% addTiles(urlTemplate="http://server.arcgisonline.com/ArcGIS/rest/services/World_Street_Map/MapServer/tile/{z}/{y}/{x}") 
    #change color to red when alert is triggered:
    #navy
    pal <- colorFactor(c("red", "darkgreen"), domain = c("normal", "alert"))
    mada_map=mada_map %>% addCircleMarkers(lng = ~Long, 
                                                       lat = ~Lat, 
                                                       weight = 1,
                                                       radius = ~myradius, 
                                                       color = ~pal(alert_status),
                                                       fillOpacity = 0.7,
                                                       popup = ~name)
    
    mada_map
    })
  output$madagascar_map2 <- renderPlot({
    if (input$diseases=="Malaria" )
    {
      if (input$Algorithmes_eval1=="Percentile" ) 
      {
        cat("display alert status into the map using percentile algorithm...\n")
        #setkey for fast merging
        setkey(sentinel_latlong,sites)
        setkey(percentile_algorithm()$percentile_alerte_currentweek,sites)
        sentinel_latlong=merge(sentinel_latlong,percentile_algorithm()$percentile_alerte_currentweek
                               ,by.x="sites",by.y = "sites")
        
      }
      if (input$Algorithmes_eval1=="MinSan" ) 
      {
        cat("display alert status into the map using MinSan algorithm...\n")
        setkey(sentinel_latlong,sites);
        setkey(minsan_algorithm()$minsan_alerte_currentweek,sites)
        sentinel_latlong=merge(sentinel_latlong,minsan_algorithm()$minsan_alerte_currentweek
                               ,by.x="sites",by.y = "sites")
      }
      if (input$Algorithmes_eval1=="Csum" ) 
      {
        cat("display alert status into the map using Csum algorithm...\n")
        setkey(sentinel_latlong,sites);
        setkey(csum_algorithm()$csum_alerte,sites)
        sentinel_latlong=merge(sentinel_latlong,csum_algorithm()$csum_alerte_currentweek
                               ,by.x="sites",by.y = "sites")
      }
      if (input$Algorithmes_eval1=="Ind" ) 
      {
        setkey(sentinel_latlong,sites);
        setkey(tdrplus_fever_ind()$tdrplus_ind_currentweek,sites)
        sentinel_latlong=merge(sentinel_latlong,tdrplus_fever_ind()$tdrplus_ind_currentweek
                               ,by.x="sites",by.y = "sites")
        cat("display alert status into the map using a simple indicator...\n")
      }
    } else {
      if (input$Algorithmes_eval2=="Percentile" ) 
      {
        cat("display alert status into the map using percentile algorithm...\n")
        #setkey for fast merging
        setkey(sentinel_latlong,sites)
        setkey(percentile_algorithm()$percentile_alerte_currentweek,sites)
        sentinel_latlong=merge(sentinel_latlong,percentile_algorithm()$percentile_alerte_currentweek
                               ,by.x="sites",by.y = "sites")
        
      }
      if (input$Algorithmes_eval2=="MinSan" ) 
      {
        cat("display alert status into the map using MinSan algorithm...\n")
        setkey(sentinel_latlong,sites);
        setkey(minsan_algorithm()$minsan_alerte_currentweek,sites)
        sentinel_latlong=merge(sentinel_latlong,minsan_algorithm()$minsan_alerte_currentweek
                               ,by.x="sites",by.y = "sites")
      }
      if (input$Algorithmes_eval2=="Csum" ) 
      {
        cat("display alert status into the map using Csum algorithm...\n")
        setkey(sentinel_latlong,sites);
        setkey(csum_algorithm()$csum_alerte,sites)
        sentinel_latlong=merge(sentinel_latlong,csum_algorithm()$csum_alerte_currentweek
                               ,by.x="sites",by.y = "sites")
      }
      
    }
      #exclude Haute Terre Centrale (High_land) from the map
      sentinel_latlong=sentinel_latlong[!(sites %in% High_land)]
      #latest verification fo radius and alert status (handle NA's)
      sentinel_latlong[is.na(alert_status)==T,alert_status:="No data"]
      sentinel_latlong[is.na(myradius)==T,myradius:=5.0]
      
      #load the map
      madagascar_layer=readRDS("madagascar.rds")
      madagascar_map2=ggmap(madagascar_layer,base_layer = ggplot(data = sentinel_latlong,aes(x=Long,y=Lat)))
      if ( length(unique(sentinel_latlong$alert_status))!=1) {
        
        madagascar_map2 = madagascar_map2 + geom_point(data = sentinel_latlong,
                                                       alpha=0.8,
                                                       aes(color=alert_status,
                                                           size=myradius
                                                           ),show.legend  = T)
        madagascar_map2 = madagascar_map2 + scale_colour_manual(values=c("#f05249", "#808284", "#69c39a"))
        #madagascar_map2 = madagascar_map2 + scale_size_discrete(guide = F)
      } else {
        madagascar_map2 = madagascar_map2 + geom_point(data = sentinel_latlong,
                                                       alpha=0.8,
                                                       aes(color=alert_status,
                                                           size=myradius),
                                                       color="blue")
      }
      
      madagascar_map2 = madagascar_map2 + scale_size_continuous(range=range(sentinel_latlong$myradius))
      return(madagascar_map2)  
      
    },height = 640)
  #Leaflet or ggmap:
  mymapchoice = reactive({
    return(input$mapchoice)
  })
  #click event handler for leaflet:
  selected_site_leaflet=eventReactive(input$madagascar_map_marker_click,{
    event= input$madagascar_map_marker_click
    print(str(event))
    return(sentinel_latlong[Long==event$lng & Lat==event$lat,get("sites")])
  },ignoreNULL=F)
 
  #click event handler (news since shiny 0.13)
 selected_site=eventReactive(input$madagascar_map2_brush, {
    Z <- brushedPoints(sentinel_latlong, 
                       input$madagascar_map2_brush, 
                       xvar="Long",yvar="Lat",allRows = TRUE)
    
    return(sentinel_latlong[Z$selected_,get("sites")])
  })
 clicked_site=eventReactive(input$madagascar_map2_click, {
   Z <- nearPoints(sentinel_latlong, 
                   input$madagascar_map2_click,
                   xvar="Long",yvar="Lat",allRows = TRUE)
   return(sentinel_latlong[Z$selected_,get("sites")])
 })
  
  #render Proportion of ILI sur Nombre de consultant
  output$propili = renderPlotly({
    #preprocess data:
    tdr_eff[,Synd_g:=GrippSusp + AutrVirResp]
    #34 sites we need:
    sites34= toupper(include[-c(1:2)])
    propili_2015 = tdr_eff[sites %in% sites34 & Annee>2014]
    stat_ili= tdr_eff[sites %in% sites34 & Annee <2015]
    #remove:
    
    
    #filter rows:
    cat("calculating prop of ILI on Nb consultation...")
     propili_2015[,prop := 100*sum(Synd_g,na.rm = T)/sum(NxConsltTotal,na.rm=T) ,by="deb_sem"]
     propili_2015= unique(propili_2015[,list(deb_sem,prop)])
     propili_2015[,weekOfday:=week(deb_sem)] #create a key to merge later 
     gc()
    cat("DONE\n")
    
  
    cat("calculating mean and max for historical ILI data...")
     setnames(stat_ili,"Semaine","weekOfday")
     stat_ili[,weekOfday:=as.numeric(weekOfday)]
     stat_ili[,mymax:=max(Synd_g,na.rm = T),by="weekOfday"]
     stat_ili[,mymean:=mean(Synd_g,na.rm = T),by="weekOfday"]
     stat_ili= unique(stat_ili[,list(weekOfday,mymax,mymean)])
    
     #create a key to merge later 
    cat("DONE\n")
    gc()
    
    
    
    #merge:
    line_width=1.0
    propili_2015= merge(propili_2015,stat_ili,
                        by.x="weekOfday",by.y="weekOfday", all.x=T)
    
    setorder(propili_2015,-deb_sem)
    
    p <- plot_ly(propili_2015, 
                 x =deb_sem, 
                 y = prop,name="prop.",
                 line = list(width=line_width))
    p = p %>% layout(title="prop of ILI among medical diagnosis",
                     font=list(size=11),
                     xaxis = list(title = "Date(Weeks)"),
                     legend = list(x = 0, 
                                  y = 10)
                    )
    p = p %>% add_trace(x=deb_sem,line = list(width=line_width),
                        y = mymean, name = "Mean<2015",visible='legendonly')
    
    
    p = p %>% add_trace(x=deb_sem,line = list(width=line_width),
                        y = mymax, name = "Max<2015",visible='legendonly')
    p
  })
  #render Syndrome Grippal, Syndrom Dengue-Like
  output$ili_graph = renderPlotly({
    #data processing:
    tdr_eff[,Synd_g := GrippSusp + AutrVirResp ]
    #34 sites we need:
    sites34= toupper(include[-c(1:2)])
    #filter rows:
    myili=tdr_eff[sites %in% sites34 & year(as.Date(deb_sem ))>=2015]
    
    p <- plot_ly(myili, x = deb_sem, type="bar",y = Synd_g,name="ILI")
    p = p %>% add_trace(x = deb_sem, type="bar",y = ArboSusp, name = "Dengue-Like")
    
    #position legend at top of the graph
    p= p %>% layout(title="ILI & Dengue-LIKE (34 sites)",
                    xaxis = list(title = "Date(weeks)"),
                    yaxis = list(title = "# Cases"),
                    legend = list(x = 0, y = 40) 
                    )
    
    p
   
    
  })
  #render weekly diseases cases for a clicked site
  output$weekly_disease_cases_persite = renderPlotly({
    
    #mydata=preprocessing()
    mydata=percentile_algorithm()$mydata
   # mydata[sites==input$diseases]
    #recupere date d'alerte  pour chaque site
    #cree une nouvelle variable = valeur de cette alerte
    mydata[,myalerte:=0]
    mydata[,myalerte:=ifelse(alert_status=="alert",occurence,NA)]
    
    
    setkey(mydata,sites)
    cat("reshape HFI to extract rainFall...")
     hfi=data.table(gather(hfi,key=sites,
                          value=myvalue,-c(code,deb_sem,type_val)))
     setkeyv(hfi,c("type_val","sites"))
    cat("DONE\n")
    
    
    if ( mymapchoice()=="other" )
    {
      sites_list=unique(c(selected_site(),clicked_site()))
      #if the user has clicked on the map:
      mydata=mydata[sites %in% sites_list ,
                    list(occurence=sum(occurence,na.rm=T)),by="code"]
      # RainFall ou Precipitation:
      cat("merge rainfall or precipitation with mydata for chosen site(s)...")
       pmm = hfi[type_val=="PRECIPITATION" & sites %in% sites_list];
       setnames(pmm,old="myvalue",new="rainFall")
       # merge with mydata
       mydata=merge(mydata,pmm,by.x=c("code"),by.y=c("code"))
       #
      cat("DONE\n")
      
    } else {
      mydata=mydata[sites %in% selected_site_leaflet(),]
      # RainFall ou Precipitation:
      cat("merge rainfall or precipitation with mydata for chosen site(s)...")
       pmm = hfi[type_val=="PRECIPITATION" & sites %in% selected_site_leaflet()];
       setnames(pmm,old="myvalue",new="rainFall")
       # merge with mydata
       pmm[,deb_sem:=NULL]
       mydata=merge(mydata,pmm,by.x=c("code"),by.y=c("code"))
      #
      cat("DONE\n")
    }
    
    #merge mild with 
    source("introducing_mild.R",local = T)
    # merge with mydata
    mydata=merge(mydata,mild[,list(code,mild_value)],
                 by.x=c("code"),by.y=c("code"),all.x=T)
    #
    #
    
    #reorder time series
    mydata[,deb_sem:=as.Date(deb_sem)]
    mydata=mydata[order(deb_sem),]
    setnames(mydata,"deb_sem","Semaine")
    #handle title programmatically (depends on user choice)
     mytitle=paste0("Weekly ",input$diseases," cases number in ",sentinel_latlong[sites==selected_site_leaflet(),get("name")])
    #
    line_width=1
    p=plot_ly(data=mydata,
              y=occurence,
              x=Semaine,name=paste0(input$diseases),
              line = list(width=line_width,color = "rgb(255, 0, 0)") #red for disease
              )
    #position legend at top of the graph
    #90th percentile as horizontal line:
    p = p %>% layout(title=mytitle,
                     font=list(size=9),
                     legend = list(x = 0, y =10 ),
                     xaxis =list(title="Weeks"),
                     yaxis =list(title="#Cases")
                     )
    p = p %>% add_trace(x=Semaine,y=myalerte,line=list(color="rgb(165,41,157)"),
                        name="percentile alerte")
    
    p = p %>% add_trace(x = Semaine, 
                        y = rainFall/10, 
                        line = list(width=line_width,color = "rgb(0, 204, 0)"),
                        name = "rainfall/10",visible='legendonly')
   
    p = p %>% add_trace(x = Semaine, y = mild_value, name = "mild",
                        color="Mild",
                        opacity=0.5,
                        colors="#132B43",
                        type="bar",
                        visible='legendonly')
    p
   
    
  })
  #Health heatmap plot with plotly and using percentile algorithm:
  output$heatmap_percentile = renderD3heatmap({
    mydata=preprocessing()
    if ( input$diseases=="Malaria" )
    {
      #depending on the algorithm chosen, display heatmap:
      if ( input$Algorithmes_eval1 == 'Percentile' ) { 
        #mydata=preprocessing()
        source("percentile.R")
        X=calculate_percentile(data=mydata,
                               week_length=input$comet_map,
                               percentile_value=input$Centile_map)$propsite_alerte_percentile
        
      }
      if ( input$Algorithmes_eval1 == 'Csum'  ) { 
        source("csum.R") 
        X=calculate_csum(data=mydata,
                         Csum_year_map=input$Csum_year_map,
                         Csum_week_map=input$Csum_week_map,
                         Sd_csum_map=input$Sd_csum_map,
                         week_choice=ifelse(Sys.Date()-as.Date(paste0(year(Sys.Date()),"-01-01"))<8
                                            ,1,week(Sys.Date())),
                         week_Csum_map=input$week_Csum_map,
                         year_choice=year(Sys.Date()),byvar="code")$propsite_alerte_csum
      }
      if ( input$Algorithmes_eval1 == 'MinSan'  ) { 
        source("minsan.R") 
        #mydata=preprocessing()
        X=calculate_minsan(data=mydata,slope_minsan=input$slope_minsan,
                           year_choice=year(Sys.Date()),
                           week_choice=ifelse(Sys.Date()-as.Date(paste0(year(Sys.Date()),"-01-01"))<8
                                              ,1,week(Sys.Date())),
                           minsan_weekrange=input$minsan_weekrange,
                           minsan_consecutive_week=input$minsan_consecutive_week,byvar="code")$propsite_alerte_minsan
        
      }
      #MUST EXIST!!
      if ( input$Algorithmes_eval1 == 'Ind') 
        { 
        X= tdrplus_fever_ind()$mydata
        #print(X)
        #Sys.sleep(25)
        }
    } else {
      if ( input$Algorithmes_eval2 == 'Percentile' ) { 
        #mydata=preprocessing()
        source("percentile.R")
        X=calculate_percentile(data=mydata,
                               week_length=input$comet_map,
                               percentile_value=input$Centile_map)$propsite_alerte_percentile
        
      }
      if ( input$Algorithmes_eval2 == 'Csum'  ) { 
        source("csum.R") 
        X=calculate_csum(data=mydata,
                         Csum_year_map=input$Csum_year_map,
                         Csum_week_map=input$Csum_week_map,
                         Sd_csum_map=input$Sd_csum_map,
                         week_choice=ifelse(Sys.Date()-as.Date(paste0(year(Sys.Date()),"-01-01"))<8
                                            ,1,week(Sys.Date())),
                         week_Csum_map=input$week_Csum_map,
                         year_choice=year(Sys.Date()),byvar="code")$propsite_alerte_csum
      }
      if ( input$Algorithmes_eval2 == 'MinSan'  ) { 
        source("minsan.R") 
        #mydata=preprocessing()
        X=calculate_minsan(data=mydata,slope_minsan=input$slope_minsan,
                           year_choice=year(Sys.Date()),
                           week_choice=ifelse(Sys.Date()-as.Date(paste0(year(Sys.Date()),"-01-01"))<8
                                              ,1,week(Sys.Date())),
                           minsan_weekrange=input$minsan_weekrange,
                           minsan_consecutive_week=input$minsan_consecutive_week,byvar="code")$propsite_alerte_minsan
        
      }
    }
    
    
    
    #selection by facies:
    if (input$Cluster_algo !="Total")
    {
      cat("you choose:",input$Cluster_algo,"\n")
      setkeyv(X,input$Cluster_algo)
      X=X[get(input$Cluster_algo)==1,]
    }
    
    cat('recode alert status...')
    setkey(X,alert_status)
    X[is.na(alert_status)==T,alert_status2:=0]
    X[alert_status=="normal",alert_status2:=1]
    X[alert_status=="alert",alert_status2:=2]
    cat('DONE\n')
    cat('spreading data...')
    X[,years:=year(deb_sem)]
    
    
    X=merge(X,sentinel_latlong[,list(sites,name)],by.x="sites",by.y="sites")
    
   
    X=X[years %in% (2016-as.numeric(input$nbyear)):2016,]
    
    #try selection using dplyr so then avoid data.frame conversion!
    #as.data.frame otherwise It won't work
    myz= as.data.frame(spread(unique(X[,list(name,deb_sem,alert_status2)]),
                              deb_sem,alert_status2))
   
    #
    cat("DONE\n")
   
    row.names(myz) <- myz$name
    d3heatmap(myz[,-1], dendrogram = "none",scale = "none",
              xaxis_font_size="9px",
              color=c("grey","darkgreen","red"))
    
  })
  #Bubble chart to display past alert:
  output$mybubble = renderPlotly({
    mydata=preprocessing()
    cat('Calculation of percentile and proportion of sites in alert begin...\n')
    source("percentile.R")
    X=calculate_percentile(data=mydata,
                           week_length=input$comet_map,
                           percentile_value=input$Centile_map)$mydata
                           
    # if (input$Cluster_algo=="Total")
    # {
       X=X[alert_status=="alert" ,]
    # } else {
      #X=X[alert_status=="alert" & get(input$Cluster_algo)==1,]
    #}
      
    X=merge(X,sentinel_latlong,by.x="sites",by.y="sites")
    setorder(X,sites,deb_sem)
    plot_ly(X, y = occurence, x=deb_sem,
            color=facies, 
            size = log(occurence+1), mode = "markers")
    #type="scatter3d"
  
  })
  #download report handler (for Malaria and Diarrhea):
  output$downloadReport <- downloadHandler(
    filename = "report.pdf",
    content = function(file) {
      file.copy('report.pdf', file)
    }
  )
  
  #Forecasting of # cases of Malaria:
  #in the future should take anykind of diseases:
  output$forecast_plot = renderPlotly({
    source("prepare_data_forecast.R",local = T)
    source("forecasting_functions.R",local = T)
    #########################################################################################################
    if ( input$mymodel=="HLT" & input$forecast_type=="retrospective")
    {
      direction="retrospective"
      load(file = "holt_retrospective.rda")
      L_preds= length(preds)
      L=length(X$occurence)
      X[,mymonth:=paste0(mymonth,"/",substr(myyear,3,4))]
      ################# plotting begins ##################################
      line_width=1.5
      cat ("MAE of holt retrospective:",mae(X$occurence[1:L_preds],preds),"\n")
      p = plot_ly(X, x=mymonth,y = occurence,
                  name="Monthly cases of Malaria",
                  line = list(width=line_width,color = "rgb(250,67,69)"))
      p = p %>% layout(legend = list(x = 0, y = 350),
                       # shapes = list(
                       #   list(type = "rect",
                       #        fillcolor = "blue", line = list(color = "blue"), opacity = 0.3,
                       #        x0 = L_preds+1, x1 =L , xref = "x",
                       #        y0 = 350, y1 = 350, yref = "y")),
                       #autosize = F,
                       #width=500,
                       #height=500,
                       title="Actual serie (Farafanga & Mananjary) vs forecasts",
                       xaxis =list(title="",dtick=3, tickangle=90),
                       yaxis =list(title="#Cases"))
      p = p %>% add_trace(x = mymonth, y = c(round(preds),X$occurence[(L_preds+1):L]),
                          name = "retrospective forecast",
                          line = list(width=line_width,color="rgb(85,135,249)") )
    } 
    #####################################################################
    if (input$mymodel=="HLT" & input$forecast_type=="prospective" ) 
    {
      direction="prospective"
      load(file = "holt_prospective.rda")
      L_preds= length(preds)
      L=length(X$occurence)
      X[,mymonth:=paste0(mymonth,"/",substr(myyear,3,4))]
      ########################### plotting begins #########################
      line_width=1.5
      cat ("MAE of holt prospective:",mae(X$occurence[(L-L_preds+1):L],preds),"\n")
      p = plot_ly(X, x=mymonth,
                  y = occurence,name="Monthly cases of Malaria",
                  line = list(width=line_width,color = "rgb(250,67,69)") )
      p = p %>% layout(legend = list(x = 0, y = 350),
                       # shapes = list(
                       #   list(type = "rect", 
                       #        fillcolor = "blue", line = list(color = "blue"), opacity = 0.3, 
                       #        x0 = 1, x1 = L-L_preds, xref = "x", 
                       #        y0 = 350, y1 = 350, yref = "y")),
                       #autosize = T,
                       #width=500,
                       #height=450,
                       title="Actual serie (Farafanga & Mananjary) vs forecasts",
                       xaxis =list(title="",dtick=3, tickangle=90),
                       yaxis =list(title="#Cases"))
      p = p %>% add_trace(x = mymonth, y = c(X$occurence[1:(L-L_preds)],round(preds)),
                          name = "prospective forecast",
                          line = list(width=line_width,color="rgb(85,135,249)") )
    }
    p
  })
}

##############################################User interface ##############
#skeleton of the user interface:
source('initialize_ui.R')
ui = list(dashboardPage(skin = "blue",
                            
  dashboardHeader(title="Sentinel surveillance",titleWidth="233"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(text="Main",tabName="mytabbox", 
               icon = icon("database")),
      diseases_choices,
      map_choices,
      myfacies_algo,
      menuItem(text="Download diseases report",
               tabName="diparam", 
               icon = icon("building")),
      menuItem(text=list("Forecasting", tags$small(class="media-heading",tags$span(class="label label-danger", "beta release"))),
               tabName="myforecast", 
               icon = icon("line-chart"))
  )), 
  dashboardBody(tabItems(
                         tabbox_item,
                         disease_item,
                         forecast_item
                         ))
),
tags$body(includeHTML("www/gears.html")),
tags$head(HTML("<link rel='stylesheet' href='www/css/mycss.css'/>")),
tags$head(HTML("<script type='text/javascript' src='js/myjs.js'></script>"))
)
#assemble UI and SERVER:
shinyApp(ui, server)








