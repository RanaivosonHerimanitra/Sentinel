#whether to run on local for debugging and development purpose
#or to pull data directly from the server:
remote_server=F;writing_to_disk=F
#load required packages:
source("libraries.R");source("var_conversion.R")
if ( exists("PaluConf")==F ) #to speed up things
{
  if ( remote_server==TRUE ) {
   # cat("connection to the server and read data using dplyr to select views and tables:\n")
  withProgress(message = 'Loading data...', value = 0, {
    n=4 #monitor progress in 4 steps:
    incProgress(1/n, detail = "establish connection to the server...")
    sentinel <- src_postgres(dbname="sentinel",
                             host = "172.16.0.230", 
                             user = "cnx_florian",
                             password = "sigflorianipm")
    data_iri_env<-src_postgres(dbname="data_iri",
                               host="172.16.0.230",
                               port=5432,
                               user="cnx_florian",
                               password="sigflorianipm")
    #should accelerate extraction
    incProgress(1/n, detail = "load malaria data...")
    
    PaluConf=fread("data/PaluConf.csv")
    max_date=max(PaluConf$deb_sem)
    PaluConf_tmp= tbl(sentinel,
                  build_sql("SELECT * FROM ",
                            "crosstab_paluconf_format"," WHERE deb_sem>=",
                            max_date
                            ))
    #transform into data.table:
    PaluConf_tmp= PaluConf_tmp  %>% data.frame() %>% data.table()
    #conversion of variables:
    var_conv(PaluConf,PaluConf_tmp)
    #rbind 02 dataframe:
    PaluConf=PaluConf[deb_sem<max_date,]
    PaluConf=(rbind(PaluConf,PaluConf_tmp))
    
    #should accelerate extraction:
    Consultations=fread("data/Consultations.csv")
    max_date=max(Consultations$deb_sem)
    Consultations_tmp= tbl(sentinel,
                       build_sql("SELECT * FROM ",
                                 "crosstab_nxconslttotal_format",
                            " WHERE deb_sem>=",max_date))
    #transform into data.table:
    Consultations_tmp= Consultations_tmp  %>% data.frame() %>% data.table()
    #conversion of variables:
    var_conv(Consultations,Consultations_tmp)
    #rbind 02 dataframe:
    Consultations=Consultations[deb_sem<max_date,]
    Consultations=(rbind(Consultations,Consultations_tmp))
    
    
    SyndF=fread("data/SyndF.csv")
    max_date=max(SyndF$deb_sem)
    SyndF_tmp=tbl(sentinel,
              build_sql("SELECT * FROM ",
                        "crosstab_syndf_format",
                        " WHERE deb_sem>=",max_date))
    #transform into data.table:
    SyndF_tmp= SyndF_tmp  %>% data.frame() %>% data.table()
    #conversion of variables:
    var_conv(SyndF,SyndF_tmp)
    #rbind 02 dataframe:
    SyndF=SyndF[deb_sem<max_date,]
    SyndF=(rbind(SyndF,SyndF_tmp))
    
    #palu autochtone:
    palu_autoch=fread("data/palu_autoch.csv")
    max_date=max(palu_autoch$deb_sem)
    palu_autoch_tmp=tbl(sentinel,
                        build_sql("SELECT * FROM ",
                                  "crosstab_autoch_format"," WHERE deb_sem>=",
                                  max_date))
    #transform into data.table:
    palu_autoch_tmp= palu_autoch_tmp  %>% data.frame() %>% data.table()
    #conversion of variables:
    var_conv(palu_autoch,palu_autoch_tmp)
    #rbind 02 dataframe:
    palu_autoch=unique(rbind(palu_autoch,palu_autoch_tmp))
    
    #
    incProgress(1/n, detail = "load diarrhea data...")
    Diarrh=fread("data/Diarrh.csv")
    max_date=max(Diarrh$deb_sem)
    Diarrh_tmp=tbl(sentinel,
               build_sql("SELECT * FROM ",
                         "crosstab_diarrh_format",
                         " WHERE deb_sem>=",max_date))
    #transform into data.table:
    Diarrh_tmp= Diarrh_tmp  %>% data.frame() %>% data.table()
    #conversion of variables:
    var_conv(Diarrh,Diarrh_tmp)
    #rbind 02 dataframe:
    Diarrh=Diarrh[deb_sem<max_date,]
    Diarrh=(rbind(Diarrh,Diarrh_tmp))
    
    
    #
    Diarrh_feb=fread("data/Diarrh_feb.csv")
    max_date= max(Diarrh_feb$deb_sem)
    Diarrh_feb_tmp=tbl(sentinel,
               build_sql("SELECT * FROM ",
                         "crosstab_diarrhfeb_format",
               " WHERE deb_sem>=",max_date))
    #transform into data.table:
    Diarrh_feb_tmp= Diarrh_feb_tmp  %>% data.frame() %>% data.table()
    #conversion of variables:
    var_conv(Diarrh_feb,Diarrh_feb_tmp)
    #remove old obs. and rbind 02 dataframe:
    Diarrh_feb=Diarrh_feb[deb_sem<max_date,]
    Diarrh_feb=unique(rbind(Diarrh_feb,Diarrh_feb_tmp))
    
    
    
    
    #Paralysie flasque aigue
    incProgress(1/n, detail = "load PFA data...")
    pfa=fread("data/pfa.csv")
    max_date=max(pfa$deb_sem)
    pfa_tmp= tbl(sentinel,
             build_sql("SELECT * FROM ",
                       "crosstab_pfa_format",
                       " WHERE deb_sem>=",max_date))
    #conversion of dataframe:
    pfa_tmp = pfa_tmp %>% data.frame() %>% data.table()
    #conversion of variables
    var_conv(pfa,pfa_tmp)
    #rbind 02 dataframe:
    pfa= pfa[deb_sem<max_date,]
    pfa=(rbind(pfa,pfa_tmp))
    
    
    
    #cat('query of TDR effectif\n')
    incProgress(1/n, detail = "load High Frequency indicators data...")
    
    tdr_eff=fread("data/tdr_eff.csv")
    max_date = max(tdr_eff$deb_sem)
    tdr_eff_tmp= tbl(sentinel,
                 build_sql('SELECT "Date" AS "deb_sem","SyndF","TestPalu","Centre2" AS "sites","Annee","Semaine","ArboSusp","GrippSusp","AutrVirResp","NxConsltTotal" FROM ',
                          "vue_csb_sms_centre_format", " WHERE 'Date'>=",max_date))
    #conversion of dataframe:
    tdr_eff_tmp = tdr_eff_tmp %>% data.frame() %>% data.table()
    #conversion of variables:
    var_conv(tdr_eff,tdr_eff_tmp)
    #rbind 02 dataframe:
    tdr_eff=tdr_eff[deb_sem<max_date,]
    tdr_eff=(rbind(tdr_eff,tdr_eff_tmp))
    
    
    
    #définir taille de cercle f(Nb total diarrhée) dans map
    ili=fread("data/ili.csv")
    max_date = max(ili$deb_sem)
    ili_tmp=tbl(sentinel,
        build_sql("SELECT * FROM ",
                  "crosstab_grippsusp_autrvirresp_format",
                  " WHERE deb_sem>=",max_date))
    #conversion of dataframe:
    ili_tmp = ili_tmp %>% data.frame() %>% data.table()
    #conversion of variables:
    var_conv(ili,ili_tmp)
    #rbind 02 dataframe:
    ili=ili[deb_sem<max_date]
    ili=(rbind(ili,ili_tmp))
    
   
    #cat('query of mild\n')
    mild<-fread("data/mild_export.csv") 
    # mild=as.data.table(as.data.frame(mild))
    
    
    
    #empilement des HFi:
    hfi=fread("data/hfi.csv")
    max_date=max(hfi$deb_sem)
    conn_hfi <- src_postgres(dbname="data_iri",
                             host = "172.16.0.230", 
                             user = "cnx_user",
                             password = "sig0000ipm")
     hfi_tmp= tbl(conn_hfi,
              build_sql("SELECT * FROM ","crosstab_iri_caid", 
                        " WHERE deb_sem>=",max_date))
     #conversion of dataframe:
     hfi_tmp = hfi_tmp %>% data.frame() %>% data.table()
     #conversion of variables:
     var_conv(hfi,hfi_tmp)
     #rbind 02 dataframe:
     hfi= hfi[deb_sem<max_date,]
     hfi=(rbind(hfi,hfi_tmp))
  })
    if (writing_to_disk==T )
    {
      #need conversion here---- NOT ACTUALLY
      #cat('writing data locally...')
      write.table(PaluConf,"data/PaluConf.csv",sep=";",row.names=F)
      write.table(Consultations,"data/Consultations.csv",sep=";",row.names=F)
      write.table(SyndF,"data/SyndF.csv",sep=";",row.names=F)
      write.table(Diarrh,"data/Diarrh.csv",sep=";",row.names=F)
      write.table(Diarrh_feb,"data/Diarrh_feb.csv",sep=";",row.names=F)
      # write.table(lst,"data/lst.csv",sep=";",row.names=F)
      #  write.table(ndvi,"data/ndvi.csv",sep=";",row.names=F)
      # write.table(pmm,"data/pmm.csv",sep=";",row.names=F)
      #write.table(caid,"data/caid.csv",sep=";",row.names=F)
      write.table(tdr_eff,"data/tdr_eff.csv",sep=";",row.names=F)
      write.table(ili,"data/ili.csv",sep=";",row.names=F)
      write.table(pfa,"data/pfa.csv",sep=";",row.names=F)
      write.table(palu_autoch,"data/palu_autoch.csv",sep=";",row.names=F)
      write.table(hfi,"data/hfi.csv",sep=";",row.names=F)
      #cat('DONE\n')
    }
    
    
    ##cat('tables in sentinel are:',src_tbls(sentinel),"\n")
    
    
  } else {
    setwd('/media/herimanitra/DONNEES/IPM_sentinelle/sentinel_hrmntr 291115/Sentinel')
    PaluConf=fread("data/PaluConf.csv")
    Consultations=fread("data/Consultations.csv")
    SyndF=fread("data/SyndF.csv")
    Diarrh=fread("data/Diarrh.csv")
    Diarrh_feb=fread("data/Diarrh_feb.csv")
    lst=fread("data/lst.csv")
    ndvi=fread("data/ndvi.csv")
    pmm=fread("data/pmm.csv")
    caid=fread("data/caid.csv")
    mild=fread("data/mild_export.csv")
    ili=fread("data/ili.csv")
    pfa=fread("data/pfa.csv")
    palu_autoch=fread("data/palu_autoch.csv")
    tdr_eff=fread("data/tdr_eff.csv");
    hfi=fread("data/hfi.csv")
  }
  
  
  #cat('query of lat/long of sites...')
  sentinel_latlong=fread("data/sentinel.csv")
  setnames(sentinel_latlong,"CODE","sites")
  sentinel_latlong[,sites:=tolower(sites)]
 
  
} else {
  #cat("No need to pull data anymore!\n")
}
