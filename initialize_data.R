###################### Initialize data to avoid error sometimes ###########
source("libraries.R")
sentinel <- src_postgres(dbname="sentinel",
                         host = "172.16.0.230", 
                         user = "cnx_florian",
                         password = "sigflorianipm")
data_iri_env<-src_postgres(dbname="data_iri",
                           host="172.16.0.230",
                           port=5432,
                           user="cnx_florian",
                           password="sigflorianipm")
conn_hfi <- src_postgres(dbname="data_iri",
                         host = "172.16.0.230", 
                         user = "cnx_user",
                         password = "sig0000ipm")

PaluConf= tbl(sentinel,
                  build_sql("SELECT * FROM ",
                            "crosstab_paluconf_format"))
#transform into data.table:
PaluConf= PaluConf  %>% data.frame() 
fwrite(PaluConf,"data/PaluConf.csv")

############################################################################
Consultations= tbl(sentinel,
                       build_sql("SELECT * FROM ",
                                 "crosstab_nxconslttotal_format"))
#transform into data.table:
Consultations= Consultations  %>% data.frame() 
fwrite(Consultations,"data/Consultations.csv")

###########################################################################

SyndF=tbl(sentinel,
              build_sql("SELECT * FROM ",
                        "crosstab_syndf_format"))
SyndF= SyndF %>% data.frame() 
fwrite(SyndF,"data/SyndF.csv")

#####################################################

Diarrh=tbl(sentinel,
               build_sql("SELECT * FROM ",
                         "crosstab_diarrh_format"))
Diarrh= Diarrh  %>% data.frame() 
fwrite(Diarrh,"data/Diarrh.csv")

#####################################################

Diarrh_feb=tbl(sentinel,
                   build_sql("SELECT * FROM ",
                             "crosstab_diarrhfeb_format"))

Diarrh_feb= Diarrh_feb  %>% data.frame() 
fwrite(Diarrh_feb,"data/Diarrh_feb.csv")


######################################################
arbosusp=tbl(sentinel,build_sql("SELECT * FROM ","crosstab_arbosusp_format"))

arbosusp= arbosusp  %>% data.frame() 
fwrite(arbosusp,"data/arbosusp.csv")

#####################################################
ili=tbl(sentinel,
            build_sql("SELECT * FROM ",
                      "crosstab_grippsusp_autrvirresp_format"))

ili= ili %>% data.frame() 
fwrite(ili,"data/ili.csv")

######################################################

hfi= tbl(conn_hfi,
             build_sql("SELECT * FROM ","crosstab_iri_caid"))
hfi= hfi %>% data.frame() 
fwrite(hfi,"data/hfi.csv")

###################################################
tdr_eff= tbl(sentinel,
                 build_sql('SELECT "Date" AS "Date","SyndF","TestPalu","Centre2" AS "sites","Annee","Semaine","ArboSusp","GrippSusp","AutrVirResp","NxConsltTotal" FROM ',
                           "vue_csb_sms_centre_format"))
tdr_eff= tdr_eff %>% data.frame() 
fwrite(tdr_eff,"data/tdr_eff.csv")
###################################################
palu_autoch=tbl(sentinel,
                    build_sql("SELECT * FROM ",
                              "crosstab_autoch_format"))
palu_autoch= palu_autoch %>% data.frame() 
fwrite(palu_autoch,"data/palu_autoch.csv")
######################################################

pfa= tbl(sentinel,
             build_sql("SELECT * FROM ",
                       "crosstab_pfa_format"))
pfa= pfa %>% data.frame() 
fwrite(pfa,"data/pfa.csv")

######################################################
missing_sent= tbl(sentinel,
                  build_sql("SELECT * FROM ",
                   "vue_csb_sms_centre_format"))
missing_sent= missing_sent %>% data.frame() 
fwrite(missing_sent,"data/missing_sent.csv")