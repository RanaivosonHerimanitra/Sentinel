########################Layers of the interactive summary report #################

####################### MAIN REPORT ####################################
historical_alert=fread("interactive_summary_report/historical_alert.csv")
#retrieve last 02 finished weeks:
mycode=unique(historical_alert$code)[1:2]
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

summary_report=list( tags$div(class="container",
                     tags$div( class="jumbotron",
                     tags$h2("Alerts summary during the last 02 weeks"),
                     tags$p(class="lead","Parameters encompass:",
                                 tags$ul(
                                   tags$li(tags$p("90th percentile is calculated for all weeks except for the ongoing week.")),
                                   tags$li(tags$p("03 consecutive weeks are needed to trigger alert when Malaria or Diarrhea exceed 90th percentile.")
                                   )))),
                     tags$div(class="row",tags$div(class="col-sm-8 blog-main",tags$div(class="blog-post",
                                                                  tags$h2(class="blog-post-title","Alerts outline:"),
                                                                  tags$p(class="blog-post-meta",mycode[1])))),
                     listOfAlerts1,
                     tags$div(class="row",tags$div(class="col-sm-8 blog-main",tags$div(class="blog-post",
                                                                                       tags$h2(class="blog-post-title","Alerts outline:"),
                                                                                       tags$p(class="blog-post-meta",mycode[2])))),
                     listOfAlerts2
))
                      
#####################################Palu autoch##################################
site20=fread("report/site20.csv")
htc_report=list(plotlyOutput("htc_report_plot"),
                tags$br(),tags$br(),tags$br(),
                selectizeInput(inputId="CSB_sites", 
                               label="Select a site", 
                               choice=site20$Centre, 
                               selected = c("Antsampandrano"), 
                               multiple = F),
                plotlyOutput("ind_htc_report_plot"))
#####################################Malaria (global)#############################
site34=fread("data/sentinel.csv")
malaria_report=list(plotlyOutput("malaria_report_plot"),
                    tags$br(),tags$br(),tags$br(),
                    selectizeInput(inputId="CSB_sites_malaria", 
                                   label="Select a site", 
                                   choice=site34$name, 
                                   selected = c("Toamasina"), 
                                   multiple = F),
                    plotlyOutput("ind_malaria_report_plot")
                   )
#####################################Diarrhea#############################
diarrhea_report=list(plotlyOutput("diarrhea_report_plot"),
                     tags$br(),tags$br(),tags$br(),
                     selectizeInput(inputId="CSB_sites_diarrhea", 
                                    label="Select a site", 
                                    choice=site34$name, 
                                    selected = c("Toamasina"), 
                                    multiple = F),
                     plotlyOutput("ind_diarrhea_report_plot")
)
#####################################Diarrhea#############################
ili_report=plotlyOutput("ili_report_plot")
#####################################Diarrhea#############################
pfa_report=plotlyOutput("pfa_report_plot")
#########################################################################
CSB=fread("report/CSB.csv")
missing_sent_report=list(tags$br(),tags$br(),
                         selectizeInput(inputId="CSB", 
                                        label="Select CSB to monitor", 
                                        choice=CSB$x, 
                                        selected = c("Ambatolahy","Ambatomiady"), 
                                        multiple = TRUE,
                                        options = list(maxItems = 6)),
                         dataTableOutput("missing_sent_report"))

