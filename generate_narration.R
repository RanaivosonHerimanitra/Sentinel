#conditions to be fullfilled before writing a report:
L_palu = length(alerte_palu[name==k,get("alert_status")])>0 
L_diar = length(alerte_diar[name==k,get("alert_status")])>0
N_fiever= alerte_manque_tdr[name==k,get("SyndF")]
N_tdr= alerte_manque_tdr[name==k,get("TestPalu")]
L_tdr = length(N_fiever)>0 & length(N_tdr)>0 
L_tana= k %in% tana_centre
L_hautplateau= k %in% tana_haut_plateau

##############################Sniffing NA values in the database #################
source("manque_bd_narration.R",local = T)
###########################################################################################
if ( k %in% c(tana_centre,tana_haut_plateau) )
{
  Nbcases= sum(percentile_palu_alerte[code==j & (name ==k),get("occurence")],na.rm = T)
} else {
  if (L_palu)
  {
    source("prep_narr_palu.R",local = T)
  }
  Nbcases=0
}

source("append_narration_subtitle.R",local = T)


#title if any of the conditions above are fullfilled
if (mylength_NA>0)
{
  cat('generate narration for ',k,' at week ',j,' because of lack of database...')
  msg_NA=  pot("  .   manque de base de données depuis ") 
  msg_NA = msg_NA + pot(ifelse (mylength_NA==1,"une semaine",paste(mylength_NA," semaines consécutives")))
  point_virgule=1
  cat('DONE','\n')
  #NEW 26mey2016:
  cat("store result in a csv...")
  msg_NA_english= ifelse(mylength_NA==1,"has a lack of data during one week",
                         paste("has a lack of data during ",mylength_NA," consecutive weeks."))
  if (historical_alert$code[1]=="" & historical_alert$sites[1]=="" & historical_alert$alert[1]=="" ) {
    historical_alert$code[1]=j
    historical_alert$sites[1]=k
    historical_alert$alert[1]=msg_NA_english
    write.table(historical_alert,"interactive_summary_report/historical_alert.csv",row.names=F,sep=";")
    historical_alert=fread("interactive_summary_report/historical_alert.csv")
   
  } else {
    DT2=data.table(code=j,sites=k,alert=msg_NA_english)
    historical_alert=rbindlist(list(historical_alert,DT2), use.names=TRUE)
    write.table(historical_alert,"interactive_summary_report/historical_alert.csv",row.names=F,sep=";")
    historical_alert=fread("interactive_summary_report/historical_alert.csv")
    
    }
  cat("DONE\n")
} else {
   msg_NA=NA
}
if (L_palu | L_diar | L_tdr )
{
  cat("at least a disease will be reported at week ",j,"\n")
  if (L_palu )
  {
    cat("at least a PALU will be reported at week ",j,"\n")
    if (L_tana | L_hautplateau)
    {
      if (Nbcases>0)
      {
        cat("generating narration for Palu in ",k," at week:",j,"\n")
          msg_palu=  pot(paste("  .  PALUDISME à ",k,"(",Nbcases," cas). Vérifier au CSB si cas importé."),en_gras_blue)
        cat('DONE\n')
        #NEW 26mey2016:
        cat("store result in a csv...")
        msg_palu_english= paste("Malaria identified","(",Nbcases," cases)",".Check whether It is imported.")
        if (historical_alert$code[1]=="" & historical_alert$sites[1]=="" & historical_alert$alert[1]=="" ) {
          historical_alert$code[1]=j
          historical_alert$sites[1]=k
          historical_alert$alert[1]=msg_palu_english
          write.table(historical_alert,"interactive_summary_report/historical_alert.csv",row.names=F,sep=";")
          historical_alert=fread("interactive_summary_report/historical_alert.csv")
         
        } else {
          DT2=data.table(code=j,sites=k,alert=msg_palu_english)
          historical_alert=rbindlist(list(historical_alert,DT2), use.names=TRUE)
          write.table(historical_alert,"interactive_summary_report/historical_alert.csv",row.names=F,sep=";")
          historical_alert=fread("interactive_summary_report/historical_alert.csv")
         
          }
        cat("DONE\n")
        
      } else {
        msg_palu=NA
      }
      
    } else {
      cat("generating narration for other sites except Tana Ville and Periphérie for Palu...")
        msg_palu=  pot("  .   est") + pot(" en ALERTE PALU",en_gras_rouge) + pot(" depuis  ")+ pot(mylength_palu,en_gras(fontsize=13) ) +pot("  semaine(s)  ")
        msg_palu= msg_palu + pot("(") + pot(paste0(myranks_palu[1],";")) + pot(paste0(myranks_palu[2],";")) + pot(paste0(myranks_palu[3],";")) + pot(paste0(myranks_palu[4],";")) + pot(paste0(myranks_palu[5],";")) + pot(paste0(myranks_palu[6],";")) + pot(paste0(myranks_palu[7],";")) + pot(paste0(myranks_palu[8],";")) + pot(paste0(myranks_palu[9],";")) + pot(paste0(myranks_palu[10],")."))
      cat('DONE\n')
      #NEW 26mey2016:
      cat("store result in a csv...")
      msg_palu_english= paste("has experienced a Malaria alert since",mylength_palu,"weeks","(", myranks_palu[1],";",myranks_palu[2],";",myranks_palu[3],";",myranks_palu[4],";",myranks_palu[5],";",myranks_palu[6],";",myranks_palu[7],";",myranks_palu[8],";",myranks_palu[9],";",myranks_palu[10],").")
      if (historical_alert$code[1]=="" & historical_alert$sites[1]=="" & historical_alert$alert[1]=="" ) {
        historical_alert$code[1]=j
        historical_alert$sites[1]=k
        historical_alert$alert[1]=msg_palu_english
        write.table(historical_alert,"interactive_summary_report/historical_alert.csv",row.names=F,sep=";")
        historical_alert=fread("interactive_summary_report/historical_alert.csv")
       
      } else {
        DT2=data.table(code=j,sites=k,alert=msg_palu_english)
        historical_alert=rbindlist(list(historical_alert,DT2), use.names=TRUE)
        write.table(historical_alert,"interactive_summary_report/historical_alert.csv",row.names=F,sep=";")
        historical_alert=fread("interactive_summary_report/historical_alert.csv")
      
        }
      cat("DONE\n")
      
    }
  } else {
    msg_palu=NA
  } 
  if (L_diar)
  {
    source("prep_narr_diar.R",local = T)
    cat("generating narration for Diarrhée for ",k,"...")
      msg_diar=  pot("  .   est") + pot(" en ALERTE DIARRHEE",en_gras_green) + pot(" depuis  ")+ pot(mylength_diar,en_gras(fontsize=13) ) +pot("  semaine(s) ")
      msg_diar= msg_diar + pot("(") + pot(paste0(myranks_diar[1],";")) + pot(paste0(myranks_diar[2],";")) + pot(paste0(myranks_diar[3],";")) + pot(paste0(myranks_diar[4],";")) + pot(paste0(myranks_diar[5],";")) + pot(paste0(myranks_diar[6],";")) + pot(paste0(myranks_diar[7],";")) + pot(paste0(myranks_diar[8],";")) + pot(paste0(myranks_diar[9],";")) + pot(paste0(myranks_diar[10],")."))
      point_virgule=1
    cat("DONE\n")
    #NEW 26mey2016:
    cat("store result in a csv...")
    msg_diar_english= paste("has experienced a Diarrhea alert since",mylength_diar,"weeks","(", myranks_diar[1],";",myranks_diar[2],";",myranks_diar[3],";",myranks_diar[4],";",myranks_diar[5],";",myranks_diar[6],";",myranks_diar[7],";",myranks_diar[8],";",myranks_diar[9],";",myranks_diar[10],").")
    if (historical_alert$code[1]=="" & historical_alert$sites[1]=="" & historical_alert$alert[1]=="" ) {
      historical_alert$code[1]=j
      historical_alert$sites[1]=k
      historical_alert$alert[1]=msg_diar_english
      write.table(historical_alert,"interactive_summary_report/historical_alert.csv",row.names=F,sep=";")
      historical_alert=fread("interactive_summary_report/historical_alert.csv")
    
    } else {
      DT2=data.table(code=j,sites=k,alert=msg_diar_english)
      historical_alert=rbindlist(list(historical_alert,DT2), use.names=TRUE)
      write.table(historical_alert,"interactive_summary_report/historical_alert.csv",row.names=F,sep=";")
      historical_alert=fread("interactive_summary_report/historical_alert.csv")
      
      }
    cat("DONE\n")
    
  } else {
    msg_diar=NA
  }
  if (L_tdr)
  {
    #to handle case when both N_fiever and N_tdr == 0
    if ( N_fiever>0 | N_tdr>0 )
    {
      cat("generating narration for TDR manquant for ",k," at week",j,"\n")
     
        tdr_manquant= pot("  .   est") + pot(" PROBABLEMENT en manque de TDR ",highlight_tdr )
        tdr_manquant=tdr_manquant + pot("cette semaine")
        tdr_manquant=tdr_manquant+ pot(paste("(",N_fiever, " cas de fièvres contre",N_tdr," TDR effectués)"))
       
      #add this line if It lasts more than a week:
      source("prep_narr_tdr.R",local = T)
      if (mylength_tdr>1 ) {
        tdr_manquant = tdr_manquant + pot(". Cela depuis ") + pot(mylength_tdr) + pot(" semaines.")
      } 
      cat("DONE\n")
      #NEW 26mey2016:
      cat("store result in a csv...")
      tdr_manquant_english= paste("probably has a lack of diagnostic kit since",mylength_tdr,"weeks (",N_fiever," fever cases against",N_tdr,"RDT test done).")
      if (historical_alert$code[1]=="" & historical_alert$sites[1]=="" & historical_alert$alert[1]=="" ) {
        historical_alert$code[1]=j
        historical_alert$sites[1]=k
        historical_alert$alert[1]=tdr_manquant_english
        write.table(historical_alert,"interactive_summary_report/historical_alert.csv",row.names=F,sep=";")
        historical_alert=fread("interactive_summary_report/historical_alert.csv")
       
      } else {
        DT2=data.table(code=j,sites=k,alert=tdr_manquant_english)
        historical_alert=rbindlist(list(historical_alert,DT2), use.names=TRUE)
        write.table(historical_alert,"interactive_summary_report/historical_alert.csv",row.names=F,sep=";")
        historical_alert=fread("interactive_summary_report/historical_alert.csv")
        
        }
      cat("DONE\n")
      
    } else {
      tdr_manquant=NA
    }
  } else {
    tdr_manquant=NA
  }
  
} else {
  #DO nothing there is no disease for the week j
  msg_palu=NA;msg_diar=NA;tdr_manquant=NA;
}
#####################################################################################
cat("Append all reports...")
if (class(msg_palu)=="pot" )
{
  doc = addParagraph(doc, msg_palu  )
}
if (class(msg_diar)=="pot" )
{
  doc = addParagraph(doc, msg_diar  )
}
if (class(tdr_manquant)=="pot" )
{
  doc = addParagraph(doc, tdr_manquant  )
}
if (class(msg_NA)=="pot" )
{
  doc = addParagraph(doc, msg_NA )
}
cat("DONE\n")
if ( exists("point_virgule") )
{
  rm(point_virgule);gc()
}
rm(msg_palu);rm(msg_diar);rm(tdr_manquant);rm(msg_NA);gc()


