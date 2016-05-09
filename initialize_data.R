###################### Initialize data to avoid error sometimes ###########
setwd("/media/herimanitra/Document/IPM_sentinelle/sentinel_hrmntr 291115/Sentinel")
source("libraries.R")
cat('connect to the server...')
sentinel <- src_postgres(dbname="***",
                         host = "***", 
                         user = "***",
                         password = "***")
data_iri_env<-src_postgres(dbname="***",
                           host="***",
                           port=000,
                           user="***",
                           password="***")
conn_hfi <- src_postgres(dbname="***",
                         host = "***", 
                         user = "***",
                         password = "***")
cat('DONE\n')

cat("retrieve and convert PaluConf into a dataframe...")
PaluConf= tbl(sentinel,
                  build_sql("SELECT * FROM ",
                            "crosstab_paluconf_format"))
PaluConf= PaluConf  %>% data.frame() 
cat("DONE\n")
cat("save PaluConf on disk...")
fwrite(PaluConf,"data/PaluConf.csv")
cat("DONE\n")

############################################################################
cat("retrieve and convert Consultations into a dataframe...")
Consultations= tbl(sentinel,
                       build_sql("SELECT * FROM ",
                                 "crosstab_nxconslttotal_format"))
#transform into data.table:
Consultations= Consultations  %>% data.frame() 
cat("DONE\n")
cat("save Consultations on disk...")
fwrite(Consultations,"data/Consultations.csv")
cat("DONE\n")

###########################################################################
cat("retrieve and convert SyndF into a dataframe...")
SyndF=tbl(sentinel,
              build_sql("SELECT * FROM ",
                        "crosstab_syndf_format"))
SyndF= SyndF %>% data.frame() 
cat("DONE\n")
cat("save SyndF on disk...")
fwrite(SyndF,"data/SyndF.csv")
cat("DONE\n")

##########################################################################
cat("retrieve and convert Diarrh into a dataframe...")

Diarrh=tbl(sentinel,
               build_sql("SELECT * FROM ",
                         "crosstab_diarrh_format"))
Diarrh= Diarrh  %>% data.frame() 
cat("DONE\n")
cat("save Diarrh on disk...")
fwrite(Diarrh,"data/Diarrh.csv")
cat("DONE\n")
#####################################################
cat("retrieve and convert Diarrh_feb into a dataframe...")

Diarrh_feb=tbl(sentinel,
                   build_sql("SELECT * FROM ",
                             "crosstab_diarrhfeb_format"))
Diarrh_feb= Diarrh_feb  %>% data.frame() 
cat("DONE\n")
cat("save Diarrh_feb on disk...")
fwrite(Diarrh_feb,"data/Diarrh_feb.csv")
cat("DONE\n")

######################################################
cat("retrieve and convert arbosusp into a dataframe...")
arbosusp=tbl(sentinel,build_sql("SELECT * FROM ","crosstab_arbosusp_format"))
arbosusp= arbosusp  %>% data.frame() 
cat("DONE\n")
cat("save arbosusp on disk...")
fwrite(arbosusp,"data/arbosusp.csv")
cat("DONE\n")
#####################################################
cat("retrieve and convert ILI into a dataframe...")
ili=tbl(sentinel,
            build_sql("SELECT * FROM ",
                      "crosstab_grippsusp_autrvirresp_format"))
ili= ili %>% data.frame() 
cat("DONE\n")
cat("save ILI on disk...")
fwrite(ili,"data/ili.csv")
cat("DONE\n")
######################################################
cat("retrieve and convert HFI into a dataframe...")
hfi= tbl(conn_hfi,
             build_sql("SELECT * FROM ","crosstab_iri_caid"))
hfi= hfi %>% data.frame() 
cat("DONE\n")
cat("save HFI on disk...")
fwrite(hfi,"data/hfi.csv")
cat("DONE\n")
###################################################
cat("retrieve and convert tdr_eff into a dataframe...")
tdr_eff= tbl(sentinel,
                 build_sql('SELECT "Date" AS "Date","SyndF","TestPalu","Centre2" AS "sites","Annee","Semaine","ArboSusp","GrippSusp","AutrVirResp","NxConsltTotal" FROM ',
                           "vue_csb_sms_centre_format"))
tdr_eff= tdr_eff %>% data.frame() 
cat("DONE\n")
cat("save tdr_eff on disk...")
fwrite(tdr_eff,"data/tdr_eff.csv")
cat("DONE\n")
###################################################
cat("retrieve and convert palu_autoch into a dataframe...")
palu_autoch=tbl(sentinel,
                    build_sql("SELECT * FROM ",
                              "crosstab_autoch_format"))
palu_autoch= palu_autoch %>% data.frame() 
cat("DONE\n")
cat("save palu_autoch on disk...")
fwrite(palu_autoch,"data/palu_autoch.csv")
cat("DONE\n")
######################################################
cat("retrieve and convert PFA into a dataframe...")
pfa= tbl(sentinel,
             build_sql("SELECT * FROM ",
                       "crosstab_pfa_format"))
pfa= pfa %>% data.frame() 
cat("DONE\n")
cat("save PFA on disk...")
fwrite(pfa,"data/pfa.csv")
cat("DONE\n")
######################################################
cat("retrieve and convert missing_sent into a dataframe...")
missing_sent= tbl(sentinel,
                  build_sql("SELECT * FROM ",
                   "vue_csb_sms_centre_format"))
missing_sent= missing_sent %>% data.frame() 
cat("DONE\n")
cat("save missing_sent on disk...")
fwrite(missing_sent,"data/missing_sent.csv")
cat("DONE\n")