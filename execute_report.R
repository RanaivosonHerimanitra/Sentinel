########################Execute report using code ####################
require(rmarkdown);require(lubridate)
if ( !file.exists("/srv/shiny-server/sentinel_hrmntr/Sentinel" ) )
{
  mypath = "/media/herimanitra/Document/IPM_sentinelle/sentinel_hrmntr 291115/Sentinel/report"
  setwd(mypath)
  
  
} else {
  mypath= "/srv/shiny-server/sentinel_hrmntr/Sentinel/report"
  setwd(mypath)
}
render(paste0(mypath,"/ILI_sentinelles.Rmd"))
if ( !file.exists("/srv/shiny-server/sentinel_hrmntr/Sentinel" ) )
{
  mypath = "/media/herimanitra/Document/IPM_sentinelle/sentinel_hrmntr 291115/Sentinel/report"
  setwd(mypath)
  
  
} else {
  mypath= "/srv/shiny-server/sentinel_hrmntr/Sentinel/report"
  setwd(mypath)
}
render(paste0(mypath,"/Diar_sentinelles.Rmd"))
if ( !file.exists("/srv/shiny-server/sentinel_hrmntr/Sentinel" ) )
{
  mypath = "/media/herimanitra/Document/IPM_sentinelle/sentinel_hrmntr 291115/Sentinel/report"
  setwd(mypath)
  
  
} else {
  mypath= "/srv/shiny-server/sentinel_hrmntr/Sentinel/report"
  setwd(mypath)
}

render(paste0(mypath,"/Palu_sentinelles.Rmd"))
if ( !file.exists("/srv/shiny-server/sentinel_hrmntr/Sentinel" ) )
{
  mypath = "/media/herimanitra/Document/IPM_sentinelle/sentinel_hrmntr 291115/Sentinel/report"
  setwd(mypath)
  
  
} else {
  mypath= "/srv/shiny-server/sentinel_hrmntr/Sentinel/report"
  setwd(mypath)
}

render(paste0(mypath,"/palu_autoch_sentinelles.Rmd"))
if ( !file.exists("/srv/shiny-server/sentinel_hrmntr/Sentinel" ) )
{
  mypath = "/media/herimanitra/Document/IPM_sentinelle/sentinel_hrmntr 291115/Sentinel/report"
  setwd(mypath)
  
  
} else {
  mypath= "/srv/shiny-server/sentinel_hrmntr/Sentinel/report"
  setwd(mypath)
}

render(paste0(mypath,"/PFA_sentinelles.Rmd"))
if ( !file.exists("/srv/shiny-server/sentinel_hrmntr/Sentinel" ) )
{
  mypath = "/media/herimanitra/Document/IPM_sentinelle/sentinel_hrmntr 291115/Sentinel/report"
  setwd(mypath)
  
  
} else {
  mypath= "/srv/shiny-server/sentinel_hrmntr/Sentinel/report"
  setwd(mypath)
}

source("miss_sent_report.R")
############################come back to main directory######################
if ( !file.exists("/srv/shiny-server/sentinel_hrmntr/Sentinel" ) )
{
  mypath = "/media/herimanitra/Document/IPM_sentinelle/sentinel_hrmntr 291115/Sentinel"
  setwd(mypath)
} else {
  mypath= "/srv/shiny-server/sentinel_hrmntr/Sentinel"
  setwd(mypath)
}

source("generate_report.R")