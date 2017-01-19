source("utils/libraries.R")
# load data
PaluConf=fread("data/PaluConf.csv")
Consultations=fread("data/Consultations.csv")
SyndF=fread("data/SyndF.csv")
palu_autoch=fread("data/palu_autoch.csv")
Diarrh=fread("data/Diarrh.csv")
Diarrh_feb=fread("data/Diarrh_feb.csv")
ili=fread("data/ili.csv")
pfa=fread("data/pfa.csv")
arbosusp=fread("data/arbosusp.csv")
tdr_eff=fread("data/tdr_eff.csv")
mild<-fread("data/mild_export.csv") 
hfi=fread("data/hfi.csv")
sentinel_latlong=fread("data/sentinel.csv")
#setnames(sentinel_latlong,"CODE","sites")

#sentinel_latlong[,sites:=tolower(sites)]
