
#define vector from which percentile distribution will be calculated:
if ( (k %in% c("Antananarivo",tana_haut_plateau,tana_centre)  )     )
{
  cat("We are in Tana!Nothing to do right now!")
  msg_palu=NA #so that It will not rewrite at the bottom of the page
} else {
  dist= percentile_palu_alerte[name==k,get("occurence")]
  cat("calculate percentile rank for PALU...")
  myranks_palu=array()
  p=1
  while ( (pos_j+p)<=length(mycode) & p!=11 )
  {
    myranks_palu[p]=perc_rank( dist,
                               percentile_palu_alerte[code==mycode[pos_j+p] & name==k,get("occurence")] )
    p=p+1
  }
  #Handle alerts duration > 10 weeks for Palu
  myranks_palu_additionnal=array(); myindex=1
  alpha=11
  if (  (pos_j+alpha)<length(mycode) )
  {
    tmp= perc_rank(dist, percentile_palu_alerte[code==mycode[pos_j+alpha] & name==k,get("occurence")])
    tmp= ifelse (is.na(tmp)== T || length(tmp)==0,0,tmp)
  }
  while ( tmp>=90 & (pos_j+alpha)<length(mycode) )
  {
    cat(k,'has reached the threshold at',alpha,'th week','with a rank:',tmp,'\n')
    myranks_palu_additionnal[myindex]=tmp
    myindex = myindex + 1
    alpha=alpha +1
    tmp= perc_rank(dist, percentile_palu_alerte[code==mycode[pos_j+alpha] & name==k,get("occurence")])
    tmp= ifelse (is.na(tmp)== T || length(tmp)==0,0,tmp)
  }
  #append alert beyond 11th week to first 10 weeks if not empty:
  if ( length(myranks_palu_additionnal)>1)
  {
    myranks_palu=c(myranks_palu,myranks_palu_additionnal)
  }
  
  myranks_palu[is.na(myranks_palu)==T]=0
  mylength_palu=length(myranks_palu)
  while( all(myranks_palu>=90) == F)
  {
    #remove last value of myranks
    myranks_palu=myranks_palu[-length(myranks_palu)]
    mylength_palu=mylength_palu-1
  }
  cat("DONE\n")
  ####################################################################################
}
  
  cat("calculate percentile rank for diarrhée...")
  dist= percentile_diar_alerte[name==k,get("occurence")]
  myranks_diar=array()
  p=1
  while ( (pos_j+p)<=length(mycode) & p!=11 )
  {
    myranks_diar[p]=perc_rank( dist,
                               percentile_diar_alerte[code==mycode[pos_j+p] & name==k,get("occurence")] )
    p=p+1
  }
  
  
  #Handle alerts duration > 10 weeks for diarrhée
  myranks_diar_additionnal=array(); myindex=1
  alpha=11
  if (  (pos_j+alpha)<length(mycode) )
  {
    tmp= perc_rank(dist, percentile_diar_alerte[code==mycode[pos_j+alpha] & name==k,get("occurence")])
    tmp= ifelse (is.na(tmp)== T || length(tmp)==0,0,tmp)
  }
  while ( tmp>=90 & (pos_j+alpha)<length(mycode) )
  {
    cat(k,'has reached the threshold at',alpha,'th week','with a rank:',tmp,'\n')
    myranks_diar_additionnal[myindex]=tmp
    myindex = myindex + 1
    alpha = alpha+1
    tmp= perc_rank(dist, percentile_diar_alerte[code==mycode[pos_j+alpha] & name==k,get("occurence")])
    tmp= ifelse (is.na(tmp)== T || length(tmp)==0,0,tmp)
    
  }
  
  #append if non vide:
  if ( length(myranks_diar_additionnal)>1)
  {
    myranks_diar=c(myranks_diar,myranks_diar_additionnal)
  }
  #
  myranks_diar[is.na(myranks_diar)==T]=0
  mylength_diar=length(myranks_diar)
  while( all(myranks_diar>=90) == F)
  {
    #remove last value of myranks
    myranks_diar=myranks_diar[-length(myranks_diar)]
    mylength_diar=mylength_diar-1
  }
  cat('DONE\n')
  
  
  
  
  #initialize with site name:
  #L_palu = length(alerte_palu[name==k,get("alert_status")])>0 & k!= "Antsirabe" & k!="Fianarantsoa"
  L_palu = length(alerte_palu[name==k,get("alert_status")])>0 
  L_diar = length(alerte_diar[name==k,get("alert_status")])>0
  cat("calculating fiever cases and TDR done for this week:",j,"...")
  N_fiever= alerte_manque_tdr[name==k,get("SyndF")]
  N_tdr= alerte_manque_tdr[name==k,get("TestPalu")]
  if ( k %in% "Antananarivo" )
  {
    Nbcases= sum(percentile_palu_alerte[code==j & (name %in% tana_centre),get("occurence")],na.rm = T)
    
  } else {
    Nbcases=0
  }
  if ( k %in% tana_haut_plateau )
  {
    Nbcases= sum(percentile_palu_alerte[code==j & name %in% k,get("occurence")],na.rm = T)
    
  } else {
    Nbcases=0
  }
  #handle NA and null
  Nbcases=ifelse (is.na(Nbcases)==T | length(Nbcases)==0,0,Nbcases)
  cat("DONE\n")
  L_tdr = length(N_fiever)>0 & length(N_tdr)>0 
  L_tana= (k %in% "Antananarivo") & (Lookingfor_tana==1)
  L_hautplateau= k %in% tana_haut_plateau
  #Append subtitle of sites in narration:
  source('append_narration_subtitle.R',local = T)
  #######################################
  #if L_palu is true then there must be an alert (except in Haute Terre)
  #so write a report:
  if ( L_palu )
  {
    # if L_tana because there is special phrase for Tana
    if ( L_tana )
    {
      if ( Nbcases>0)
      {
        cat("sites:",k,"\n")
        #special report for Antananarivo sites sentinel:
        cat("generating narration for Palu in ",k,"...")
        #Nbcases= sum(percentile_palu_alerte[code==j & name %in% tana_centre,get("occurence")],na.rm = T)
        msg_palu=  pot("  .  PALUDISME à Antananarivo ",en_gras_rouge) + pot("(") + pot(Nbcases) + pot(" cas). Vérifier au CSB si cas importé.")
        doc <- addParagraph(doc, msg_palu)
        cat('DONE\n')
        if (k=="Antananarivo")
        {
          Lookingfor_tana=0
        }
       
      }
      msg_palu=NA #so that It will not rewrite at the bottom of the page
    } else {
      if (L_hautplateau )
      {
        if (Nbcases>0)
        {
          Lookingfor_tana=1
          cat("sites:",k,"\n")
          #special report for Antananarivo sites sentinel:
          cat("generating narration for Palu in ",k,"...")
          msg_palu=  pot("  .  ALERTE PALU",en_gras_rouge) + pot("(") + pot(Nbcases) + pot(" cas). Vérifier au CSB si cas importé.")
          doc <- addParagraph(doc, msg_palu)
          cat('DONE\n')
        } else {
          msg_palu=NA
        }
        
      } else {
        cat("generating narration for other sites except Tanaa Ville and Periphrie for Palu...")
        msg_palu=  pot("  .   est") + pot(" en ALERTE PALU",en_gras_rouge) + pot(" dépassant le seuil depuis  ")+ pot(mylength_palu,en_gras(fontsize=13) ) +pot("  semaines consécutives ")
        msg_palu= msg_palu + pot("(") + pot(paste0(myranks_palu[1],";")) + pot(paste0(myranks_palu[2],";")) + pot(paste0(myranks_palu[3],";")) + pot(paste0(myranks_palu[4],";")) + pot(paste0(myranks_palu[5],";"))
        msg_palu = msg_palu + pot(paste0(myranks_palu[6],";")) + pot(paste0(myranks_palu[7],";")) + pot(paste0(myranks_palu[8],";")) + pot(paste0(myranks_palu[9],";")) + pot(paste0(myranks_palu[10],")."))
        cat('DONE\n')
      }
      
    }
  } else {
    msg_palu=NA
  }
######################################################################################
  if ( L_diar )
  {
    cat("generating narration for Diarrhée...")
    msg_diar=  pot("  .   est") + pot(" en ALERTE DIARRHEE",en_gras_green) + pot(" dépassant le seuil depuis  ")+ pot(mylength_diar,en_gras(fontsize=13) ) +pot("  semaines consécutives ")
    msg_diar= msg_diar + pot("(") + pot(paste0(myranks_diar[1],";")) + pot(paste0(myranks_diar[2],";")) + pot(paste0(myranks_diar[3],";")) + pot(paste0(myranks_diar[4],";")) + pot(paste0(myranks_diar[5],";"))
    msg_diar = msg_diar + pot(paste0(myranks_diar[6],";")) + pot(paste0(myranks_diar[7],";")) + pot(paste0(myranks_diar[8],";")) + pot(paste0(myranks_diar[9],";")) + pot(paste0(myranks_diar[10],")."))
    cat("DONE\n")
  } else {
    msg_diar=NA
  }
######################################################################################
  if ( L_tdr )
  {
    #to handle when both N_fiever and N_tdr == 0
    if ( N_fiever>0 | N_tdr>0 )
    {
      cat("generating narration for TDR manquant...")
      tdr_manquant= pot("  .   est") + pot(" PROBABLEMENT en manque de TDR ",highlight_tdr )
      tdr_manquant=tdr_manquant + pot("cette semaine")
      tdr_manquant=tdr_manquant+ pot(paste("(",N_fiever, " cas de fièvres contre",N_tdr," TDR effectués)"))
      cat("DONE\n")
    } else {
      tdr_manquant=NA
    }
    
  }   else {
    tdr_manquant=NA
  }
#####################################################################################
  cat("Append all reports...")
  if ( class(msg_palu)=="pot" ) { doc <- addParagraph(doc, msg_palu) }
  if ( class(msg_diar)=="pot" ) { doc <- addParagraph(doc, msg_diar) }
  if ( class(tdr_manquant)=="pot" ) { doc <- addParagraph(doc, tdr_manquant) }
  cat("DONE\n")
  
  rm(msg_palu);rm(msg_diar);rm(tdr_manquant);gc()
  

