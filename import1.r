#whether to run on local for debugging and development purpose
#or to pull data directly from the server:
remote_server=F;writing_to_disk=F
#load required packages:
source("libraries.R")
if ( exists("PaluConf")==F ) #to speed up things
{
  if ( remote_server==TRUE ) {
    cat("connection to the server and read data using dplyr to select views and tables:\n")
    sentinel <- src_postgres(dbname="sentinel",
                             host = "172.16.0.230", 
                             user = "cnx_florian",
                             password = "sigflorianipm")
    data_iri_env<-src_postgres(dbname="data_iri",
                               host="172.16.0.230",
                               port=5432,
                               user="cnx_florian",
                               password="sigflorianipm")
    
    PaluConf= tbl(sentinel,
                  build_sql('SELECT * FROM "crosstab_paluconf_format"'))
    
    Consultations= tbl(sentinel,
                       build_sql('SELECT * FROM "crosstab_nxconslttotal_format"'))
    
    SyndF=tbl(sentinel,
              build_sql('SELECT * FROM "crosstab_syndf_format"'))
    Diarrh=tbl(sentinel,
               build_sql('SELECT * FROM "crosstab_diarrh_format"'))
    Diarrh_feb=tbl(sentinel,
               build_sql('SELECT * FROM "crosstab_diarrhfeb_format"'))
    #définir taille de cercle f(Nb total diarrhée) dans map
    
    #cat('query of Land Surface Temperature(lst)\n')
    lst=tbl(data_iri_env,
            build_sql('SELECT * FROM "groupe_lst_day_format"'))
    
    #cat('query of NDVI\n')
    ndvi=tbl(data_iri_env,
             build_sql('SELECT * FROM "groupe_ndvi_format"'))
    
    #cat('query of Precipitation(pmm)\n')
    pmm=tbl(data_iri_env,
            build_sql('SELECT * FROM "groupe_precipitation_format"'))
    
    #cat('query of CAID\n')
    caid=tbl(sentinel,"caid")
    
    #cat('query of mild\n')
    mild<-fread("mild_export.csv")
    
    #cat('query of TDR effectif\n')
    tdr_eff= tbl(sentinel,
                    build_sql('SELECT * FROM "vue_csb_sms_centre_format"'))
    
    if (writing_to_disk==T )
    {
      #cat('writing data locally...')
      write.table(PaluConf,"PaluConf.csv",sep=";",row.names=F)
      write.table(Consultations,"Consultations.csv",sep=";",row.names=F)
      write.table(SyndF,"SyndF.csv",sep=";",row.names=F)
      write.table(Diarrh,"Diarrh.csv",sep=";",row.names=F)
      write.table(Diarrh_feb,"Diarrh_feb.csv",sep=";",row.names=F)
      write.table(lst,"lst.csv",sep=";",row.names=F)
      write.table(ndvi,"ndvi.csv",sep=";",row.names=F)
      write.table(pmm,"pmm.csv",sep=";",row.names=F)
      write.table(caid,"caid.csv",sep=";",row.names=F)
      write.table(tdr_eff,"tdr_eff.csv",sep=";",row.names=F)
      #cat('DONE\n')
    }
    
    
    ##cat('tables in sentinel are:',src_tbls(sentinel),"\n")
    
    
  } else {
    setwd('/media/herimanitra/DONNEES/IPM_sentinelle/sentinel_hrmntr 291115/Sentinel')
    PaluConf=fread("PaluConf.csv")
    Consultations=fread("Consultations.csv")
    SyndF=fread("SyndF.csv")
    Diarrh=fread("Diarrh.csv")
    Diarrh_feb=fread("Diarrh_feb.csv")
    lst=fread("lst.csv")
    ndvi=fread("ndvi.csv")
    pmm=fread("pmm.csv")
    caid=fread("caid.csv")
    mild=fread("mild_export.csv")
    tdr_eff=fread("tdr_eff.csv");
    setnames(tdr_eff,"Centre2","sites")
    tdr_eff[,code:=paste0(Annee,"_",Semaine)]
    tdr_eff=tdr_eff[,list(sum(SyndF,na.rm = T),sum(TestPalu,na.rm = T)),by="sites,code"]
    setnames(tdr_eff,old=c("V1","V2"), new = c("SyndF","TestPalu"))
    tdr_eff=tdr_eff[,list(sites,code,SyndF,TestPalu)]
    tdr_eff[,sites:=tolower(sites)]
  }
  
  
  #cat('query of lat/long of sites...')
  sentinel_latlong=fread("sentinel.csv");
  setnames(sentinel_latlong,"CODE","sites")
  sentinel_latlong[,sites:=tolower(sites)]
  #to handle reporting for Antananarivo:
  # sentinel_latlong[sites %in% c("cda","bhk"),name:="Antananarivo"]
  #cat("DONE\n")
  
} else {
  #cat("No need to pull data anymore!\n")
}
