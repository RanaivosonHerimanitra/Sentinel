 giveme_propsite_alerte = function (data=PaluConf,byvar="code",algo="percentile")
 {
#    if (algo=="percentile")
#    {
#      cat("Nombre de sites ayant dépassé le seuil (alert==1) par semaine (code):...\n")
#      Nbsite_beyond=data[is.na(occurence)==F & alert==1,length(unique(sites)),by=byvar]
#      setnames(Nbsite_beyond,"V1","eff_beyond")
#      cat("Nombre de sites ayant donné des données(is.na(occurrence)==F) par semaine...\n")
#      Nbsite_withdata=data[is.na(occurence)==F,length(unique(sites)),by=byvar]
#      setnames(Nbsite_withdata,"V1","eff_total")
#      #MERGE to give proportion of sites beyond n_percentile per week:
#      propsite_alerte_percentile=merge(x=Nbsite_withdata,y=Nbsite_beyond,by.x=byvar,by.y=byvar,all.x=T)
#      #calculate prop and change NA to zero:
#      propsite_alerte_percentile[,prop:=ifelse(is.na(eff_beyond/eff_total)==T,0.0,eff_beyond/eff_total)]
#      #merge with deb_sem to reorder time series:
#      propsite_alerte_percentile=merge(propsite_alerte_percentile,PaluConf[,list(code,deb_sem)],
#                                       by.x=byvar,by.y=byvar)
#      rm(Nbsite_withdata);rm(Nbsite_beyond);gc()
#      return(propsite_alerte_percentile) 
#    }
#    if (algo=="minsan") {
#      cat("Nombre de sites ayant dépassé le seuil (alert_status==alert) par semaine (code):...\n")
#      Nbsite_beyond=data[is.na(occurence)==F & alert_status=="alert",length(unique(sites)),by=byvar]
#      setnames(Nbsite_beyond,"V1","eff_beyond")
#      cat("Nombre de sites ayant donné des données(is.na(occurrence)==F) par semaine...\n")
#      Nbsite_withdata=data[is.na(occurence)==F,length(unique(sites)),by=byvar]
#      setnames(Nbsite_withdata,"V1","eff_total")
#      #MERGE to give proportion of sites beyond n_percentile per week:
#      propsite_alerte_minsan=merge(x=Nbsite_withdata,y=Nbsite_beyond,by.x=byvar,by.y=byvar,all.x=T)
#      #calculate prop and change NA to zero:
#      propsite_alerte_minsan[,prop:=ifelse(is.na(eff_beyond/eff_total)==T,0.0,eff_beyond/eff_total)]
#      #merge with deb_sem to reorder time series:
#      propsite_alerte_minsan=merge(propsite_alerte_minsan,PaluConf[,list(code,deb_sem)],
#                                       by.x=byvar,by.y=byvar)
#      rm(Nbsite_withdata);rm(Nbsite_beyond);gc()
#      return(propsite_alerte_minsan) 
#    }
#    if (algo=="csum") {
#      cat("Nombre de sites ayant dépassé le seuil (alert_status==alert csum algorithm) par semaine (code):...\n")
#      Nbsite_beyond=data[is.na(occurence)==F & alert_status=="alert",length(unique(sites)),by=byvar]
#      setnames(Nbsite_beyond,"V1","eff_beyond")
#      cat("Nombre de sites ayant donné des données(is.na(occurrence)==F) par semaine...\n")
#      Nbsite_withdata=data[is.na(occurence)==F,length(unique(sites)),by=byvar]
#      setnames(Nbsite_withdata,"V1","eff_total")
#      #MERGE to give proportion of sites beyond n_percentile per week:
#      propsite_alerte_csum=merge(x=Nbsite_withdata,y=Nbsite_beyond,by.x=byvar,by.y=byvar,all.x=T)
#      #calculate prop and change NA to zero:
#      propsite_alerte_csum[,prop:=ifelse(is.na(eff_beyond/eff_total)==T,0.0,eff_beyond/eff_total)]
#      #merge with deb_sem to reorder time series:
#      propsite_alerte_csum=merge(propsite_alerte_csum,PaluConf[,list(code,deb_sem)],
#                                   by.x=byvar,by.y=byvar)
#      rm(Nbsite_withdata);rm(Nbsite_beyond);gc()
#      return(propsite_alerte_csum) 
#    }
  
   
 }