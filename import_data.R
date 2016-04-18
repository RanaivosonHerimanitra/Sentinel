#whether to run on local for debugging and development purpose
#or to pull data directly from the server:
remote_server=F;writing_to_disk=F
#load required packages:
source("libraries.R");source("var_conversion.R")
if ( exists("PaluConf")==F ) #to speed up things
{
  if ( remote_server==TRUE ) {
   # cat("connection to the server and read data using dplyr to select views and tables:\n")
    sentinel <- src_postgres(dbname="sentinel",
                             host = "172.16.0.230", 
                             user = "cnx_florian",
                             password = "sigflorianipm")
    data_iri_env<-src_postgres(dbname="data_iri",
                               host="172.16.0.230",
                               port=5432,
                               user="cnx_florian",
                               password="sigflorianipm")
    
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
    #tdr_eff=fread("data/tdr_eff.csv")
    ################################################################"
    max_date=max(PaluConf$deb_sem)
    #should accelerate extraction:
    PaluConf_tmp= tbl(sentinel,
                  build_sql("SELECT * FROM ",
                            "crosstab_paluconf_format"," WHERE deb_sem>=",
                            max_date
                            ))
    
    if (dim(PaluConf_tmp)[2]>0)
    {
     
      #transform into data.table:
      PaluConf_tmp= PaluConf_tmp  %>% data.frame() %>% data.table()
      PaluConf_tmp[,deb_sem:=as.character(deb_sem)]
      fwrite(PaluConf_tmp,"data/PaluConf_tmp.csv")
      PaluConf_tmp=fread("data/PaluConf_tmp.csv",colClasses = c(deb_sem="character"))
      PaluConf=PaluConf[deb_sem<max_date,]
      PaluConf=rbind(PaluConf,PaluConf_tmp)
      PaluConf[,deb_sem:=as.Date(deb_sem)]
      setorder(PaluConf,-deb_sem)
      
      # #conversion of variables:
      # var_conv(PaluConf,PaluConf_tmp)
      # #rbind 02 dataframe:
      # PaluConf=PaluConf[deb_sem<max_date,]
      # PaluConf=(rbind(PaluConf,PaluConf_tmp))
      # setorder(PaluConf,-deb_sem)
    }
    ######################################
    max_date=max(Consultations$deb_sem)
    Consultations_tmp= tbl(sentinel,
                       build_sql("SELECT * FROM ",
                                 "crosstab_nxconslttotal_format",
                            " WHERE deb_sem>=",max_date))
    
    if (dim(Consultations_tmp)[2]>0)
    {
      #transform into data.table:
      Consultations_tmp= Consultations_tmp  %>% data.frame() %>% data.table()
      Consultations_tmp[,deb_sem:=as.character(deb_sem)]
      fwrite(Consultations_tmp,"data/Consultations_tmp.csv")
      Consultations_tmp=fread("data/Consultations_tmp.csv",colClasses = c(deb_sem="character"))
      Consultations=Consultations[deb_sem<max_date,]
      Consultations=rbind(Consultations,Consultations_tmp)
      Consultations[,deb_sem:=as.Date(deb_sem)]
      setorder(Consultations,-deb_sem)
      # #conversion of variables:
      # var_conv(Consultations,Consultations_tmp)
      # #rbind 02 dataframe:
      # Consultations=Consultations[deb_sem<max_date,]
      # Consultations=(rbind(Consultations,Consultations_tmp)) 
      # setorder(Consultations,-deb_sem)
    }
    ############################################################
    max_date=max(SyndF$deb_sem)
    SyndF_tmp=tbl(sentinel,
              build_sql("SELECT * FROM ",
                        "crosstab_syndf_format",
                        " WHERE deb_sem>=",max_date))
    
    if ( dim(SyndF_tmp)[2]>0 )
    {
      SyndF_tmp= SyndF_tmp  %>% data.frame() %>% data.table()
      SyndF_tmp[,deb_sem:=as.character(deb_sem)]
      fwrite(SyndF_tmp,"data/SyndF_tmp.csv")
      SyndF_tmp=fread("data/SyndF_tmp.csv",colClasses = c(deb_sem="character"))
      SyndF=SyndF[deb_sem<max_date,]
      SyndF=rbind(SyndF,SyndF_tmp)
      SyndF[,deb_sem:=as.Date(deb_sem)]
      setorder(SyndF,-deb_sem)
      
      # #transform into data.table:
      # SyndF_tmp= SyndF_tmp  %>% data.frame() %>% data.table()
      # #conversion of variables:
      # var_conv(SyndF,SyndF_tmp)
      # #rbind 02 dataframe:
      # SyndF=SyndF[deb_sem<max_date,]
      # SyndF=(rbind(SyndF,SyndF_tmp))
      # setorder(SyndF,-deb_sem)
    }
    ###########################################################
    #palu autochtone:
   
    max_date=max(palu_autoch$deb_sem)
    palu_autoch_tmp=tbl(sentinel,
                        build_sql("SELECT * FROM ",
                                  "crosstab_autoch_format"," WHERE deb_sem>=",
                                  max_date))
    
    if (dim(palu_autoch_tmp)[2]>0 )
    {
      palu_autoch_tmp= palu_autoch_tmp  %>% data.frame() %>% data.table()
      palu_autoch_tmp[,deb_sem:=as.character(deb_sem)]
      fwrite(palu_autoch_tmp,"data/palu_autoch_tmp.csv")
      palu_autoch_tmp=fread("data/palu_autoch_tmp.csv",colClasses = c(deb_sem="character"))
      palu_autoch=palu_autoch[deb_sem<max_date,]
      palu_autoch=rbind(palu_autoch,palu_autoch_tmp)
      palu_autoch[,deb_sem:=as.Date(deb_sem)]
      setorder(palu_autoch,-deb_sem)
      
      # #transform into data.table:
      # palu_autoch_tmp= palu_autoch_tmp  %>% data.frame() %>% data.table()
      # #conversion of variables:
      # var_conv(palu_autoch,palu_autoch_tmp)
      # #rbind 02 dataframe:
      # palu_autoch=unique(rbind(palu_autoch,palu_autoch_tmp))
      # setorder(palu_autoch,-deb_sem)
    }
    ################################################################
    
    max_date=max(Diarrh$deb_sem)
    Diarrh_tmp=tbl(sentinel,
               build_sql("SELECT * FROM ",
                         "crosstab_diarrh_format",
                         " WHERE deb_sem>=",max_date))
        
    if ( dim(Diarrh_tmp)[2]>0 )
    {
      Diarrh_tmp= Diarrh_tmp  %>% data.frame() %>% data.table()
      Diarrh_tmp[,deb_sem:=as.character(deb_sem)]
      fwrite(Diarrh_tmp,"data/Diarrh_tmp.csv")
      Diarrh_tmp=fread("data/Diarrh_tmp.csv",colClasses = c(deb_sem="character"))
      Diarrh=Diarrh[deb_sem<max_date,]
      Diarrh=rbind(Diarrh,Diarrh_tmp)
      Diarrh[,deb_sem:=as.Date(deb_sem)]
      setorder(Diarrh,-deb_sem)
      
      # #transform into data.table:
      # Diarrh_tmp= Diarrh_tmp  %>% data.frame() %>% data.table()
      # #conversion of variables:
      # var_conv(Diarrh,Diarrh_tmp)
      # #rbind 02 dataframe:
      # Diarrh=Diarrh[deb_sem<max_date,]
      # Diarrh=(rbind(Diarrh,Diarrh_tmp))
      # setorder(Diarrh,-deb_sem)
    }
    
    
    ##########################################################"
   
    max_date= max(Diarrh_feb$deb_sem)
    Diarrh_feb_tmp=tbl(sentinel,
               build_sql("SELECT * FROM ",
                         "crosstab_diarrhfeb_format",
               " WHERE deb_sem>=",max_date))
    
    if ( dim(Diarrh_feb_tmp)[2]>0 )
    {
      Diarrh_feb_tmp= Diarrh_feb_tmp  %>% data.frame() %>% data.table()
      Diarrh_feb_tmp[,deb_sem:=as.character(deb_sem)]
      fwrite(Diarrh_feb_tmp,"data/Diarrh_feb_tmp.csv")
      Diarrh_feb_tmp=fread("data/Diarrh_feb_tmp.csv",colClasses = c(deb_sem="character"))
      Diarrh_feb=Diarrh_feb[deb_sem<max_date,]
      Diarrh_feb=rbind(Diarrh_feb,Diarrh_feb_tmp)
      Diarrh_feb[,deb_sem:=as.Date(deb_sem)]
      setorder(Diarrh_feb,-deb_sem)
      
      # #transform into data.table:
      # Diarrh_feb_tmp= Diarrh_feb_tmp  %>% data.frame() %>% data.table()
      # #conversion of variables:
      # var_conv(Diarrh_feb,Diarrh_feb_tmp)
      # #remove old obs. and rbind 02 dataframe:
      # Diarrh_feb=Diarrh_feb[deb_sem<max_date,]
      # Diarrh_feb=unique(rbind(Diarrh_feb,Diarrh_feb_tmp))
      # setorder(Diarrh_feb,-deb_sem)
    }
    ##################################################################
    #Paralysie flasque aigue
   
    max_date=max(pfa$deb_sem)
    pfa_tmp= tbl(sentinel,
             build_sql("SELECT * FROM ",
                       "crosstab_pfa_format",
                       " WHERE deb_sem>=",max_date))
    
    if (dim(pfa_tmp)[2]>0)
    {
      pfa_tmp= pfa_tmp  %>% data.frame() %>% data.table()
      pfa_tmp[,deb_sem:=as.character(deb_sem)]
      fwrite(pfa_tmp,"data/pfa_tmp.csv")
      pfa_tmp=fread("data/pfa_tmp.csv",colClasses = c(deb_sem="character"))
      pfa=pfa[deb_sem<max_date,]
      pfa=rbind(pfa,pfa_tmp)
      pfa[,deb_sem:=as.Date(deb_sem)]
      setorder(pfa,-deb_sem)
      # #conversion of dataframe:
      # pfa_tmp = pfa_tmp %>% data.frame() %>% data.table()
      # #conversion of variables
      # var_conv(pfa,pfa_tmp)
      # #rbind 02 dataframe:
      # pfa= pfa[deb_sem<max_date,]
      # pfa=(rbind(pfa,pfa_tmp))
      # setorder(pfa,-deb_sem)
    }
    
    max_date=max(arbosusp$deb_sem)
    # Arbosusp
     arbosusp_tmp=tbl(sentinel,build_sql("SELECT * FROM ","crosstab_arbosusp_format",
                                     " WHERE deb_sem>=",max_date))
     if (dim(arbosusp_tmp)[2]>0)
     {
       arbosusp_tmp= arbosusp_tmp  %>% data.frame() %>% data.table()
       arbosusp_tmp[,deb_sem:=as.character(deb_sem)]
       fwrite(arbosusp_tmp,"data/arbosusp_tmp.csv")
       arbosusp_tmp=fread("data/arbosusp_tmp.csv",colClasses = c(deb_sem="character"))
       arbosusp=arbosusp[deb_sem<max_date,]
       arbosusp=rbind(arbosusp,arbosusp_tmp)
       arbosusp[,deb_sem:=as.Date(deb_sem)]
       setorder(arbosusp,-deb_sem)
       
       # #conversion of dataframe:
       # arbosusp_tmp = arbosusp_tmp %>% data.frame() %>% data.table()
       # #conversion of variables
       # var_conv(pfa,arbosusp_tmp)
       # #rbind 02 dataframe:
       # arbosusp= arbosusp[deb_sem<max_date,]
       # arbosusp=rbind(arbosusp,arbosusp_tmp)
       # setorder(arbosusp,-deb_sem)
     }
    
    #
    
    # max_date = max(tdr_eff$deb_sem)
    # tdr_eff_tmp= tbl(sentinel,
    #              build_sql('SELECT "Date" AS "deb_sem","SyndF","TestPalu","Centre2" AS "sites","Annee","Semaine","ArboSusp","GrippSusp","AutrVirResp","NxConsltTotal" FROM ',
    #                       "vue_csb_sms_centre_format", " WHERE 'Date'>=",max_date))
    # 
    # if (dim(tdr_eff_tmp)[2]>0)
    # {
    #   #conversion of dataframe:
    #   tdr_eff_tmp = tdr_eff_tmp %>% data.frame() %>% data.table()
    #   #conversion of variables:
    #   var_conv(tdr_eff,tdr_eff_tmp)
    #   #rbind 02 dataframe:
    #   tdr_eff=tdr_eff[deb_sem<max_date,]
    #   tdr_eff=(rbind(tdr_eff,tdr_eff_tmp))
    #   setorder(tdr_eff,-deb_sem)
    # }
    #définir taille de cercle f(Nb total diarrhée) dans map
    
    max_date = max(ili$deb_sem)
    ili_tmp=tbl(sentinel,
        build_sql("SELECT * FROM ",
                  "crosstab_grippsusp_autrvirresp_format",
                  " WHERE deb_sem>=",max_date))
    
    if (dim(ili_tmp)[2]>0)
    {
      ili_tmp= ili_tmp  %>% data.frame() %>% data.table()
      ili_tmp[,deb_sem:=as.character(deb_sem)]
      fwrite(ili_tmp,"data/ili_tmp.csv")
      ili_tmp=fread("data/ili_tmp.csv",colClasses = c(deb_sem="character"))
      ili=ili[deb_sem<max_date,]
      ili=rbind(ili,ili_tmp)
      ili[,deb_sem:=as.Date(deb_sem)]
      setorder(ili,-deb_sem)
      
      # #conversion of dataframe:
      # ili_tmp = ili_tmp %>% data.frame() %>% data.table()
      # #conversion of variables:
      # var_conv(ili,ili_tmp)
      # #rbind 02 dataframe:
      # ili=ili[deb_sem<max_date]
      # ili=(rbind(ili,ili_tmp))
      # setorder(ili,-deb_sem)
    }
    
    
   
    #cat('query of mild\n')
    mild<-fread("data/mild_export.csv") 
   
    
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
     
     if ( dim(hfi_tmp)[2]>0 )
     {
       hfi_tmp= hfi_tmp  %>% data.frame() %>% data.table()
       hfi_tmp[,deb_sem:=as.character(deb_sem)]
       fwrite(hfi_tmp,"data/hfi_tmp.csv")
       hfi_tmp=fread("data/hfi_tmp.csv",colClasses = c(deb_sem="character"))
       hfi=hfi[deb_sem<max_date,]
       hfi=rbind(hfi,hfi_tmp)
       hfi[,deb_sem:=as.Date(deb_sem)]
       setorder(hfi,-deb_sem)
       # #conversion of dataframe:
       # hfi_tmp = hfi_tmp %>% data.frame() %>% data.table()
       # #append only if non empty datatable 
       # #conversion of variables:
       # var_conv(hfi,hfi_tmp)
       # #rbind 02 dataframe:
       # hfi= hfi[deb_sem<max_date,]
       # hfi=(rbind(hfi,hfi_tmp))
       # setorder(hfi,-deb_sem)
     }
    if (writing_to_disk==T )
    {
      #need conversion here---- NOT ACTUALLY
      #cat('writing data locally...')
      fwrite(PaluConf,"data/PaluConf.csv",sep=";")
      fwrite(Consultations,"data/Consultations.csv",sep=";")
      fwrite(SyndF,"data/SyndF.csv",sep=";")
      fwrite(Diarrh,"data/Diarrh.csv",sep=";")
      fwrite(Diarrh_feb,"data/Diarrh_feb.csv",sep=";")
      #fwrite(tdr_eff,"data/tdr_eff.csv",sep=";")
      fwrite(ili,"data/ili.csv",sep=";")
      fwrite(pfa,"data/pfa.csv",sep=";")
      fwrite(palu_autoch,"data/palu_autoch.csv",sep=";")
      fwrite(hfi,"data/hfi.csv",sep=";")
      fwrite(arbosusp_tmp,"data/arbosusp.csv",sep=";")
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
    tdr_eff=fread("data/tdr_eff.csv")
    hfi=fread("data/hfi.csv")
  }
  
  
  #cat('query of lat/long of sites...')
  sentinel_latlong=fread("data/sentinel.csv")
  setnames(sentinel_latlong,"CODE","sites")
  sentinel_latlong[,sites:=tolower(sites)]
 
  
} else {
  #cat("No need to pull data anymore!\n")
}
