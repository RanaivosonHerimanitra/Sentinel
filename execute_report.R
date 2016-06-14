########################Execute report using code ####################
require(rmarkdown);require(lubridate)
if ( getwd()!="/srv/shiny-server/sentinel_hrmntr/Sentinel" )
{
  mypath= "/srv/shiny-server/sentinel_hrmntr/Sentinel/report"
  setwd(mypath)
} else {
  mypath = "/media/herimanitra/Document/IPM_sentinelle/sentinel_hrmntr 291115/Sentinel/report"
  setwd(mypath)
}
render("ILI_sentinelles.Rmd")
rm(list = ls())
render("Diar_sentinelles.Rmd")
rm(list = ls())
render("Palu_sentinelles.Rmd")
rm(list = ls())
render("palu_autoch_sentinelles.Rmd")
rm(list = ls())
render("PFA_sentinelles.Rmd")
rm(list = ls())
source("miss_sent_report.R")
setwd(mypath)
rm(list = ls())
source("generate_report.R")