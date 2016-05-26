########################Layers of the interactive summary report #######
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
    listOfAlerts2[[ix]]=tags$p(class="lead",style="color:red;font-weight: bold;",paste(historical_alert2[ix,sites],historical_alert2[ix,alert]))
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
                      
                       # <div class="row marketing">
                       # <div class="col-lg-6">
                       # <h4>Subheading</h4>
                       # <p>Donec id elit non mi porta gravida at eget metus. Maecenas faucibus mollis interdum.</p>
                       # 
                       # <h4>Subheading</h4>
                       # <p>Morbi leo risus, porta ac consectetur ac, vestibulum at eros. Cras mattis consectetur purus sit amet fermentum.</p>
                       # 
                       # <h4>Subheading</h4>
                       # <p>Maecenas sed diam eget risus varius blandit sit amet non magna.</p>
                       # </div>
                       # 
                       # <div class="col-lg-6">
                       # <h4>Subheading</h4>
                       # <p>Donec id elit non mi porta gravida at eget metus. Maecenas faucibus mollis interdum.</p>
                       # 
                       # <h4>Subheading</h4>
                       # <p>Morbi leo risus, porta ac consectetur ac, vestibulum at eros. Cras mattis consectetur purus sit amet fermentum.</p>
                       # 
                       # <h4>Subheading</h4>
                       # <p>Maecenas sed diam eget risus varius blandit sit amet non magna.</p>
                       # </div>
                       # </div>