########################Layers of the interactive summary report #################

####################### MAIN REPORT ####################################
historical_alert=fread("interactive_summary_report/historical_alert.csv")
#retrieve last month alert:
mycode=unique(historical_alert$code)[1:8]
historical_alert1=historical_alert[code %in% mycode[1]]
#initialize a list to store alert in html boostrap-ish:
listOfAlerts1=list()
for ( ix in 1:nrow(historical_alert1) )
{
  #apply style conditionnally:
  if (length(grep("diagnostic kit",historical_alert1[ix,alert]))>0) {
    listOfAlerts1[[ix]]=tags$p(class="lead",style="color:#ffb90f;font-weight: bold;" ,paste(historical_alert1[ix,sites],historical_alert1[ix,alert]))
  } 
  if (length(grep("Malaria identified",historical_alert1[ix,alert]))>0) {
    listOfAlerts1[[ix]]=tags$p(class="lead",style="color:blue;font-weight: bold;",paste(historical_alert1[ix,sites],historical_alert1[ix,alert]))
  }
  if (length(grep("Malaria alert",historical_alert1[ix,alert]))>0) {
    listOfAlerts1[[ix]]=tags$p(class="lead",style="color:red;font-weight: bold;",paste(historical_alert1[ix,sites],historical_alert1[ix,alert]))
  }
  if (length(grep("Diarrhea alert",historical_alert1[ix,alert]))>0) {
    listOfAlerts1[[ix]]=tags$p(class="lead",style="color:darkgreen;font-weight: bold;",paste(historical_alert1[ix,sites],historical_alert1[ix,alert]))
  }
  if (length(grep("has a lack of data",historical_alert1[ix,alert]))>0) {
    listOfAlerts1[[ix]]=tags$p(class="lead",paste(historical_alert1[ix,sites],historical_alert1[ix,alert]))
  }
}
historical_alert2=historical_alert[code %in% mycode[2]]
#initialize a list to store alert in html boostrap-ish:
listOfAlerts2=list()
for ( ix in 1:nrow(historical_alert2) )
{
  #apply style conditionnally:
  if (length(grep("diagnostic kit",historical_alert2[ix,alert]))>0) {
    listOfAlerts2[[ix]]=tags$p(class="lead",style="color:#ffb90f;font-weight: bold;" ,paste(historical_alert2[ix,sites],historical_alert2[ix,alert]))
  } 
  if (length(grep("Malaria identified",historical_alert2[ix,alert]))>0) {
    listOfAlerts2[[ix]]=tags$p(class="lead",style="color:blue;font-weight: bold;",paste(historical_alert2[ix,sites],historical_alert2[ix,alert]))
  }
  if (length(grep("Malaria alert",historical_alert2[ix,alert]))>0) {
    listOfAlerts2[[ix]]=tags$p(class="lead",style="color:red;",paste(historical_alert2[ix,sites],historical_alert2[ix,alert]))
  }
  if (length(grep("Diarrhea alert",historical_alert2[ix,alert]))>0) {
    listOfAlerts2[[ix]]=tags$p(class="lead",style="color:darkgreen;font-weight: bold;",paste(historical_alert2[ix,sites],historical_alert2[ix,alert]))
  }
  if (length(grep("has a lack of data",historical_alert2[ix,alert]))>0) {
    listOfAlerts2[[ix]]=tags$p(class="lead",paste(historical_alert2[ix,sites],historical_alert2[ix,alert]))
  }
}
##################################################################################
historical_alert3=historical_alert[code %in% mycode[3]]
#initialize a list to store alert in html boostrap-ish:
listOfAlerts3=list()
for ( ix in 1:nrow(historical_alert3) )
{
  #apply style conditionnally:
  if (length(grep("diagnostic kit",historical_alert3[ix,alert]))>0) {
    listOfAlerts3[[ix]]=tags$p(class="lead",style="color:#ffb90f;font-weight: bold;" ,paste(historical_alert3[ix,sites],historical_alert3[ix,alert]))
  } 
  if (length(grep("Malaria identified",historical_alert3[ix,alert]))>0) {
    listOfAlerts3[[ix]]=tags$p(class="lead",style="color:blue;font-weight: bold;",paste(historical_alert3[ix,sites],historical_alert3[ix,alert]))
  }
  if (length(grep("Malaria alert",historical_alert3[ix,alert]))>0) {
    listOfAlerts3[[ix]]=tags$p(class="lead",style="color:red;",paste(historical_alert3[ix,sites],historical_alert3[ix,alert]))
  }
  if (length(grep("Diarrhea alert",historical_alert3[ix,alert]))>0) {
    listOfAlerts3[[ix]]=tags$p(class="lead",style="color:darkgreen;font-weight: bold;",paste(historical_alert3[ix,sites],historical_alert3[ix,alert]))
  }
  if (length(grep("has a lack of data",historical_alert3[ix,alert]))>0) {
    listOfAlerts3[[ix]]=tags$p(class="lead",paste(historical_alert3[ix,sites],historical_alert3[ix,alert]))
  }
}
###################################################################################
historical_alert4=historical_alert[code %in% mycode[4]]
#initialize a list to store alert in html boostrap-ish:
listOfAlerts4=list()
for ( ix in 1:nrow(historical_alert4) )
{
  #apply style conditionnally:
  if (length(grep("diagnostic kit",historical_alert4[ix,alert]))>0) {
    listOfAlerts4[[ix]]=tags$p(class="lead",style="color:#ffb90f;font-weight: bold;" ,paste(historical_alert4[ix,sites],historical_alert4[ix,alert]))
  } 
  if (length(grep("Malaria identified",historical_alert4[ix,alert]))>0) {
    listOfAlerts4[[ix]]=tags$p(class="lead",style="color:blue;font-weight: bold;",paste(historical_alert4[ix,sites],historical_alert4[ix,alert]))
  }
  if (length(grep("Malaria alert",historical_alert4[ix,alert]))>0) {
    listOfAlerts4[[ix]]=tags$p(class="lead",style="color:red;",paste(historical_alert4[ix,sites],historical_alert4[ix,alert]))
  }
  if (length(grep("Diarrhea alert",historical_alert4[ix,alert]))>0) {
    listOfAlerts4[[ix]]=tags$p(class="lead",style="color:darkgreen;font-weight: bold;",paste(historical_alert4[ix,sites],historical_alert4[ix,alert]))
  }
  if (length(grep("has a lack of data",historical_alert4[ix,alert]))>0) {
    listOfAlerts4[[ix]]=tags$p(class="lead",paste(historical_alert4[ix,sites],historical_alert4[ix,alert]))
  }
}

historical_alert5=historical_alert[code %in% mycode[5]]
#initialize a list to store alert in html boostrap-ish:
listOfAlerts5=list()
for ( ix in 1:nrow(historical_alert5) )
{
  #apply style conditionnally:
  if (length(grep("diagnostic kit",historical_alert5[ix,alert]))>0) {
    listOfAlerts5[[ix]]=tags$p(class="lead",style="color:#ffb90f;font-weight: bold;" ,
                               paste(historical_alert5[ix,sites],historical_alert5[ix,alert]))
  } 
  if (length(grep("Malaria identified",historical_alert1[ix,alert]))>0) {
    listOfAlerts5[[ix]]=tags$p(class="lead",style="color:blue;font-weight: bold;",
                               paste(historical_alert5[ix,sites],historical_alert5[ix,alert]))
  }
  if (length(grep("Malaria alert",historical_alert5[ix,alert]))>0) {
    listOfAlerts5[[ix]]=tags$p(class="lead",style="color:red;font-weight: bold;",
                               paste(historical_alert5[ix,sites],historical_alert5[ix,alert]))
  }
  if (length(grep("Diarrhea alert",historical_alert5[ix,alert]))>0) {
    listOfAlerts5[[ix]]=tags$p(class="lead",style="color:darkgreen;font-weight: bold;",
                               paste(historical_alert5[ix,sites],historical_alert5[ix,alert]))
  }
  if (length(grep("has a lack of data",historical_alert5[ix,alert]))>0) {
    listOfAlerts5[[ix]]=tags$p(class="lead",
                               paste(historical_alert5[ix,sites],historical_alert5[ix,alert]))
  }
}
historical_alert6=historical_alert[code %in% mycode[6]]
#initialize a list to store alert in html boostrap-ish:
listOfAlerts6=list()
for ( ix in 1:nrow(historical_alert6) )
{
  #apply style conditionnally:
  if (length(grep("diagnostic kit",historical_alert6[ix,alert]))>0) {
    listOfAlerts6[[ix]]=tags$p(class="lead",style="color:#ffb90f;font-weight: bold;" ,
                               paste(historical_alert6[ix,sites],historical_alert6[ix,alert]))
  } 
  if (length(grep("Malaria identified",historical_alert6[ix,alert]))>0) {
    listOfAlerts6[[ix]]=tags$p(class="lead",style="color:blue;font-weight: bold;",
                               paste(historical_alert6[ix,sites],historical_alert6[ix,alert]))
  }
  if (length(grep("Malaria alert",historical_alert6[ix,alert]))>0) {
    listOfAlerts6[[ix]]=tags$p(class="lead",style="color:red;",
                               paste(historical_alert6[ix,sites],historical_alert6[ix,alert]))
  }
  if (length(grep("Diarrhea alert",historical_alert6[ix,alert]))>0) {
    listOfAlerts6[[ix]]=tags$p(class="lead",style="color:darkgreen;font-weight: bold;",
                               paste(historical_alert6[ix,sites],historical_alert6[ix,alert]))
  }
  if (length(grep("has a lack of data",historical_alert6[ix,alert]))>0) {
    listOfAlerts6[[ix]]=tags$p(class="lead",
                               paste(historical_alert6[ix,sites],historical_alert6[ix,alert]))
  }
}
##################################################################################
historical_alert7=historical_alert[code %in% mycode[7]]
#initialize a list to store alert in html boostrap-ish:
listOfAlerts7=list()
for ( ix in 1:nrow(historical_alert3) )
{
  #apply style conditionnally:
  if (length(grep("diagnostic kit",historical_alert7[ix,alert]))>0) {
    listOfAlerts3[[ix]]=tags$p(class="lead",style="color:#ffb90f;" ,
                               paste(historical_alert7[ix,sites],historical_alert7[ix,alert]))
  } 
  if (length(grep("Malaria identified",historical_alert7[ix,alert]))>0) {
    listOfAlerts7[[ix]]=tags$p(class="lead",style="color:blue;font-weight: bold;",
                               paste(historical_alert7[ix,sites],historical_alert7[ix,alert]))
  }
  if (length(grep("Malaria alert",historical_alert7[ix,alert]))>0) {
    listOfAlerts7[[ix]]=tags$p(class="lead",style="color:red;",
                               paste(historical_alert7[ix,sites],historical_alert7[ix,alert]))
  }
  if (length(grep("Diarrhea alert",historical_alert7[ix,alert]))>0) {
    listOfAlerts7[[ix]]=tags$p(class="lead",style="color:darkgreen;font-weight: bold;",
                               paste(historical_alert7[ix,sites],historical_alert7[ix,alert]))
  }
  if (length(grep("has a lack of data",historical_alert7[ix,alert]))>0) {
    listOfAlerts7[[ix]]=tags$p(class="lead",
                               paste(historical_alert7[ix,sites],historical_alert7[ix,alert]))
  }
}
###################################################################################
historical_alert8=historical_alert[code %in% mycode[8]]
#initialize a list to store alert in html boostrap-ish:
listOfAlerts8=list()
for ( ix in 1:nrow(historical_alert8) )
{
  #apply style conditionnally:
  if (length(grep("diagnostic kit",historical_alert8[ix,alert]))>0) {
    listOfAlerts8[[ix]]=tags$p(class="lead",style="color:#ffb90f;" ,
                               paste(historical_alert8[ix,sites],historical_alert8[ix,alert]))
  } 
  if (length(grep("Malaria identified",historical_alert8[ix,alert]))>0) {
    listOfAlerts8[[ix]]=tags$p(class="lead",style="color:blue;font-weight: bold;",
                               paste(historical_alert8[ix,sites],historical_alert8[ix,alert]))
  }
  if (length(grep("Malaria alert",historical_alert8[ix,alert]))>0) {
    listOfAlerts8[[ix]]=tags$p(class="lead",style="color:red;",
                               paste(historical_alert8[ix,sites],historical_alert8[ix,alert]))
  }
  if (length(grep("Diarrhea alert",historical_alert8[ix,alert]))>0) {
    listOfAlerts8[[ix]]=tags$p(class="lead",style="color:darkgreen;font-weight: bold;",
                               paste(historical_alert8[ix,sites],historical_alert8[ix,alert]))
  }
  if (length(grep("has a lack of data",historical_alert8[ix,alert]))>0) {
    listOfAlerts8[[ix]]=tags$p(class="lead",
                               paste(historical_alert8[ix,sites],historical_alert8[ix,alert]))
  }
}
####################################################################################
summary_report=list( tags$div(class="container",
                     tags$div( class="jumbotron",
                     tags$h2("Alerts summary during the last 02 months"),
                     tags$p(class="lead","Parameters encompass:",
                                 tags$ul(
                                   tags$li(tags$p("90th percentile is calculated for all weeks except for the ongoing week.")),
                                   tags$li(tags$p("03 consecutive weeks are needed to trigger alert when Malaria or Diarrhea cases exceed the 90th percentile.")
                                   )))),
                     tags$div(class="row",tags$div(class="col-sm-8 blog-main",tags$div(class="blog-post",
                                                                  tags$h2(class="blog-post-title",paste("Alerts outline for ",mycode[1],":") 
                                                                  )))),
                     listOfAlerts1,
                     tags$div(class="row",tags$div(class="col-sm-8 blog-main",tags$div(class="blog-post",
                                                                                       tags$h2(class="blog-post-title",paste("Alerts outline for ",mycode[2],":") 
                                                                                       )))),
                     listOfAlerts2,
                     tags$div(class="row",tags$div(class="col-sm-8 blog-main",tags$div(class="blog-post",
                                                                                       tags$h2(class="blog-post-title",paste("Alerts outline for ",mycode[3],":") 
                                                                                       )))),
                     listOfAlerts3,
                     tags$div(class="row",tags$div(class="col-sm-8 blog-main",tags$div(class="blog-post",
                                                                                       tags$h2(class="blog-post-title",paste("Alerts outline for ",mycode[4],":") 
                                                                                       )))),
                     listOfAlerts4,
                     tags$div(class="row",tags$div(class="col-sm-8 blog-main",tags$div(class="blog-post",
                                                                                       tags$h2(class="blog-post-title",paste("Alerts outline for ",mycode[5],":") 
                                                                                       )))),
                     listOfAlerts5,
                     tags$div(class="row",tags$div(class="col-sm-8 blog-main",tags$div(class="blog-post",
                                                                                       tags$h2(class="blog-post-title",paste("Alerts outline for ",mycode[6],":") 
                                                                                       )))),
                     listOfAlerts6,
                     tags$div(class="row",tags$div(class="col-sm-8 blog-main",tags$div(class="blog-post",
                                                                                       tags$h2(class="blog-post-title",paste("Alerts outline for ",mycode[7],":") 
                                                                                       )))),
                     listOfAlerts7,
                     tags$div(class="row",tags$div(class="col-sm-8 blog-main",tags$div(class="blog-post",
                                                                                       tags$h2(class="blog-post-title",paste("Alerts outline for ",mycode[8],":") 
                                                                                       )))),
                     listOfAlerts8
))
                      
#####################################Palu autoch##################################
site20=fread("report/site20.csv")
htc_report=list(plotOutput("htc_report_plot"),
                #plotlyOutput("htc_report_plot"),
                tags$br(),tags$br(),tags$br(),
                tags$div(class="container",
                         tags$div(class="starter-template",
                                  tags$h2("Malaria Autochtone in HTC sites"))),
                dataTableOutput("table_htc_report"),
                tags$br(),tags$br(),tags$br(),
                selectizeInput(inputId="CSB_sites", 
                               label="Select a site", 
                               choice=site20$Centre, 
                               selected = c("Morarano"), 
                               multiple = F),
                plotOutput("ind_htc_report_plot")
                #plotlyOutput("ind_htc_report_plot")
                )
#####################################Malaria (global)#############################
site34=fread("data/sentinel.csv")
malaria_report=list(plotOutput("malaria_report_plot"),
  #plotlyOutput("malaria_report_plot"),
                    tags$br(),tags$br(),tags$br(),
                    tags$div(class="container",
                             tags$div(class="starter-template",
                                      tags$h2("Malaria in the network (no HTC sites)"))),
                    dataTableOutput("table_malaria_report"),
                    tags$br(),tags$br(),
                    tags$div(class="container",
                             tags$div(class="starter-template",
                                      tags$h2("Fever in the network (no HTC sites)"))),
                    dataTableOutput("table_fever_report"),
                    tags$br(),tags$br(),
                    selectizeInput(inputId="CSB_sites_malaria", 
                                   label="Select a site", 
                                   choice=site34$name, 
                                   selected = c("Toamasina"), 
                                   multiple = F),
  plotOutput("ind_malaria_report_plot")
                   # plotlyOutput("ind_malaria_report_plot")
                   )
#####################################Diarrhea#############################
diarrhea_report=list(plotOutput("diarrhea_report_plot"),
                     #plotlyOutput("diarrhea_report_plot"),
                     tags$br(),tags$br(),tags$br(),
                     tags$div(class="container",
                              tags$div(class="starter-template",
                                       tags$h2("Diarrhea in the network (no HTC sites)"))),
                     dataTableOutput("table_diarrhea_report"),
                     tags$br(),tags$br(),
                     tags$div(class="container",
                              tags$div(class="starter-template",
                                       tags$h2("Diarrhea febrile in the network (no HTC sites)"))),
                     dataTableOutput("table_diarrhea_febrile_report"),
                     tags$br(),tags$br(),
                     selectizeInput(inputId="CSB_sites_diarrhea", 
                                    label="Select a site", 
                                    choice=site34$name, 
                                    selected = c("Toamasina"), 
                                    multiple = F),
                     plotOutput("ind_diarrhea_report_plot")
                     #plotlyOutput("ind_diarrhea_report_plot")
                     )
#####################################Diarrhea#############################
ili_report=list(plotOutput("ili_report_plot"),
  #plotlyOutput("ili_report_plot"),
                tags$br(),tags$br(),tags$br(),
                tags$div(class="container",
                         tags$div(class="starter-template",
                                  tags$h2("ILI in the network (no HTC sites)"))),
                dataTableOutput("table_ili_report"),
                tags$br(),tags$br(),
                selectizeInput(inputId="CSB_sites_ili", 
                               label="Select a site", 
                               choice=site34$name, 
                               selected = c("Toamasina"), 
                               multiple = F),
  plotOutput("ind_ili_report_plot")
                #plotlyOutput("ind_ili_report_plot")
  )
                
#####################################Diarrhea#############################
pfa_report=list(plotOutput("pfa_report_plot"),
  #plotlyOutput("pfa_report_plot"),
                tags$br(),tags$br(),tags$br(),
                tags$div(class="container",
                         tags$div(class="starter-template",
                                  tags$h2("AFP in the network"))),
                dataTableOutput("table_pfa_report"),
                selectizeInput(inputId="CSB_sites_pfa",
                               label="Select a site", 
                               choice=c(site20$Centre,site34$name), 
                               selected = c("Toamasina"), 
                               multiple = F),
  plotOutput("ind_pfa_report_plot")
                #plotlyOutput("ind_pfa_report_plot")
                )
#########################################################################
CSB=fread("report/CSB.csv")
missing_sent_report=list(tags$br(),tags$br(),
                         selectizeInput(inputId="CSB_missing_sent", 
                                        label="Select CSB to monitor", 
                                        choice=CSB$x, 
                                        selected = c("Ambatolahy","Ambatomiady"), 
                                        multiple = TRUE,
                                        options = list(maxItems = 6)),
                         dataTableOutput("missing_sent_report"))

