################################ Generate weekly reporting ####################
library(ReporteRs)

doc <- docx() 

doc=addImage(doc, "/media/herimanitra/Document/IPM_sentinelle/sentinel_hrmntr 291115/Sentinel/report/logo.png",
         par.properties = parProperties(text.align = "left", padding = 5), 
         width=5,
         height=1.5)

# Change the default font size and font family
options('ReporteRs-fontsize'=11, 'ReporteRs-default-font'='Arial')

# Add a formatted paragraph of texts
doc = addTitle(doc, "Surveillance sentinelle", level=1)
doc = addParagraph(doc , pot( Sys.Date(), textItalic() ) )
doc = addParagraph(doc, "        ")
# Define a style to highlight a text
highlight_tdr <- textProperties(color='#ffb90f',
                                font.size = 11,
                                font.weight = 'bold', 
                                font.family = 'Courier New' )
en_gras = function ( fontsize=11,fontweight='bold',fontfamily='Courier New')
{
  return(textProperties(font.size = fontsize, 
                 font.weight = fontweight, 
                 font.family = fontfamily))
}
en_gras_green = textProperties(color='#006400',
                               font.size = 11, 
                               font.weight = 'bold', 
                               font.family = 'Courier New')
en_gras_rouge = textProperties(color='red',
                         font.size = 11, 
                         font.weight = 'bold', 
                         font.family = 'Courier New')
en_gras_blue = textProperties(color='blue',
                               font.size = 11, 
                               font.weight = 'bold', 
                               font.family = 'Courier New')
sous_titre1= pot("Rapport sur les cas historiques de Malaria et de Diarrhée à Madagascar:",
                     format=textBoldItalic(underline = TRUE ))
doc <- addParagraph(doc, sous_titre1)
sous_titre2 = pot("Les paramètres d'alerte sont:")
doc <- addParagraph(doc, sous_titre2)

alert_parameter1 = pot("Le 90ième percentile calculé sur toutes les semaines historiques exceptées la semaine en cours.")
alert_parameter2 = pot("03 semaines consécutives sont nécessaire pour déclencher une ")+pot("alerte",format=en_gras()) 
alert_parameter2=alert_parameter2 + pot(" (i.e lorsque les cas de Malaria ou de Diarrhée dépassent le 90ième percentile durant ces 03 semaines consécutives).")
#alert_parameter3 = pot ("Les bases de données manquantes dans la dernière semaine reflètent la semaine en cours d’acquisition.")

alert_parameter= set_of_paragraphs(alert_parameter1,alert_parameter2)


doc=addParagraph( doc, value = alert_parameter, stylename="BulletList")

######################### R code to generate the report ##########################
require(tidyr);source("import_data.R");
source("percentile.R");
source("preprocessing.R");
#source("tdrplus.R");


mydata=preprocessing_disease(include_all_sites=F)
#PaluConf_tdr= tdr_malaria(); 

#append all 54 sites in sentinel_latlong
sentinel=fread("data/sentinel_codes.csv");sentinel
setnames(sentinel,c("Centre","Code"),c("name","sites") )
sentinel[,sites:=tolower(sites)]
sentinel_latlong=rbind(sentinel_latlong,sentinel[!(sites %in% sentinel_latlong$sites)],fill=T)

#generate reports starting from 2012 only:
PaluConf_tdr=tdr_eff
Malaria=mydata[["Malaria"]]

diarrh=mydata[["Diarrhea"]]


percentile_palu_alerte=calculate_percentile(data=Malaria,
                                            week_length=3,
                                            percentile_value=90)$mydata
percentile_diar_alerte=calculate_percentile(data=diarrh,
                                            week_length=3,
                                            percentile_value=90)$mydata
                                            

percentile_palu_alerte=merge(percentile_palu_alerte,sentinel_latlong,
                             by.x=c("sites"),by.y=c("sites"),all.x=T)
percentile_diar_alerte=merge(percentile_diar_alerte,sentinel_latlong,
                             by.x=c("sites"),by.y=c("sites"),all.x=T)
#generate reports starting from 2012 only:
# percentile_diar_alerte=percentile_diar_alerte[years>=2012]
# percentile_palu_alerte=percentile_palu_alerte[years>=2012]
#################prepare data to detect lack of RDT #######
PaluConf_tdr[,code:= paste0(Annee,"_",ifelse(nchar(Semaine)<2,paste0("0",Semaine),Semaine))]
PaluConf_tdr[,SyndF:=sum(SyndF,na.rm=T),by="sites,code"]
PaluConf_tdr[,TestPalu:=sum(TestPalu,na.rm=T),by="sites,code"]

#new: (evaluate whether lack of TDR)
PaluConf_tdr[,manque_tdr:=SyndF - TestPalu]
#only select those with possible lack of TDR (RDT)
PaluConf_tdr = PaluConf_tdr[manque_tdr>0]
#remove unused variables
PaluConf_tdr[,Date:=NULL]
PaluConf_tdr[,ArboSusp:=NULL]
PaluConf_tdr[,GrippSusp:=NULL]
PaluConf_tdr[,AutrVirResp:=NULL]
PaluConf_tdr[,NxConsltTotal:=NULL]
#
PaluConf_tdr=unique(PaluConf_tdr,by=NULL)
PaluConf_tdr[,sites:=tolower(sites)]
PaluConf_tdr=merge(PaluConf_tdr,sentinel_latlong[,list(sites,name)],
                    by.x=c("sites"),by.y=c("sites"))


##########################################################################

tana_centre = c("Manjakaray","Andohatapenaka","Tsaralalana",
                "Behoririka")
tana_haut_plateau= c("Fianarantsoa","Antsirabe","Anjozorobe")

#reorder time (very important step!)
setkey(percentile_palu_alerte,code)
setorder(percentile_palu_alerte,sites,-deb_sem)
setkey(percentile_diar_alerte,code)
setorder(percentile_diar_alerte,sites,-deb_sem)
setorder(PaluConf_tdr,sites,-Annee,-Semaine)
#
mycode=unique(c(percentile_palu_alerte$code,
                percentile_diar_alerte$code))
                #PaluConf_tdr$code))
#remove ongoing week:
ongoing_week= paste0(year(Sys.Date()),"_",isoweek(Sys.Date()))
mycode=mycode[mycode!=ongoing_week]
#
setkey(percentile_diar_alerte,code)
setorder(percentile_diar_alerte,sites,-deb_sem)
#function that calculates percentile rank
perc_rank = function(x,x0) {f=ecdf(x);return(round(100*f(x0)) )}
# initialize document:
mydocument = list()
#initialize a data.frame to be saved in a csv for historical alerts:
historical_alert=data.table(code="",sites="",alert="")
#report only years>=2012
for ( j in mycode[as.numeric(substr(mycode,1,4))>=2012][1:10] )
{
  cat("writing report for Semaine épidémiologique:",j,'\n')
  semaine = as.numeric(unlist(strsplit(j,"_"))[2])
  semaine= ifelse(semaine<10,paste0("0",semaine),semaine)
  annee = as.numeric(unlist(strsplit(j,"_"))[1])
  alerte_palu= percentile_palu_alerte[code == j & alert_status=="alert",c("code","name",grep("^alert",names(percentile_palu_alerte),value=T)),with=F]
  alerte_diar= percentile_diar_alerte[code == j & alert_status=="alert",c("code","name",grep("^alert",names(percentile_diar_alerte),value=T)),with=F] 
  #15h39 (5 avril,'16)==>manque_tdr==1
  alerte_manque_tdr=PaluConf_tdr[code==j & manque_tdr>0,list(code,name,manque_tdr,TestPalu,SyndF)]
  
  #currently (8juin2016) cannot handle NA in HTC sites because there is no HTC site in percentile_palu_alerte
  #palu_NA= percentile_palu_alerte[code == j & is.na(occurence)==T,get("name")]
  #diar_NA= percentile_diar_alerte[code == j & is.na(occurence)==T,get("name")]
  #so switch to :
  palu_NA =sentinel_latlong[sites %in% names(PaluConf)[which(is.na(PaluConf[code == j ])==T)],get("name") ]
  diar_NA =sentinel_latlong[sites %in%  names(Diarrh)[which(is.na(Diarrh[code == j ])==T)],get("name") ]
  #
  mydate = pot(paste0(annee,"-",semaine,":"),format=textBoldItalic(underline = TRUE ))
  #add space between semaine épidémiologique:
  doc <- addParagraph(doc, "          ")
  doc <- addParagraph(doc, mydate)
  doc <- addParagraph(doc, "          ")
  #position of the current epidemiologic in j:
  pos_j=which(mycode %in% j)
  #generate narration per site, per week :
  mysites=unique(c(alerte_palu$name,
                   alerte_diar$name,
                   alerte_manque_tdr$name,palu_NA,diar_NA))
 
  # k in mysites ---
  #unlist(lapply(mysites, function(k) source("generate_narration.R",local = T)));
  for ( k in mysites )
  {
    source("generate_narration.R",local = T)
  }
}

cat("Writing log into a csv...")
write.table(historical_alert,"interactive_summary_report/historical_alert.csv",row.names = F,sep=";")
cat("DONE\n")
cat("Writing document to a word document...")
writeDoc(doc, file = "report/report.docx")
#sudo apt-get install unoconv
system("doc2pdf report/report.docx") #write in pdf using cli command
cat('DONE\n')

