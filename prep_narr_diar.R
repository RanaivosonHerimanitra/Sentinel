###########################Script for preparing narration for Diarrhée ##############
cat("calculate percentile rank for diarrhée...")
dist= percentile_diar_alerte[name==k & is.na(occurence)==F,get("occurence")]
myranks_diar=array()
p=1-1
while ( (pos_j+p)<=length(mycode) & p!=11-1 )
{
  myranks_diar[p+1]=perc_rank( dist,
                             percentile_diar_alerte[code==mycode[pos_j+p] & name==k,get("occurence")] )
  p=p+1
}


#Handle alerts duration > 10 weeks for diarrhée
myranks_diar_additionnal=array(); myindex=1
alpha=11-1
if (  (pos_j+alpha)<length(mycode) )
{
  tmp= perc_rank(dist, percentile_diar_alerte[code==mycode[pos_j+alpha] & name==k,get("occurence")])
  tmp= ifelse (is.na(tmp)== T || length(tmp)==0,0,tmp)
  while ( tmp>=90 & (pos_j+alpha)<length(mycode) )
  {
    cat(k,'has reached the threshold at',alpha,'th week','with a rank:',tmp,'\n')
    myranks_diar_additionnal[myindex]=tmp
    myindex = myindex + 1
    alpha = alpha+1
    tmp= perc_rank(dist, percentile_diar_alerte[code==mycode[pos_j+alpha] & name==k,get("occurence")])
    tmp= ifelse (is.na(tmp)== T || length(tmp)==0,0,tmp)
  }
}

#append if non vide:
if ( length(myranks_diar_additionnal)>1)
{
  myranks_diar=c(myranks_diar,myranks_diar_additionnal)
}
#
tmp=myranks_diar
tmp[is.na(tmp)==T]=0
mylength_diar=length(myranks_diar)
while( all(tmp>=90) == F & length(tmp)!=3  )
{
  #remove last value of myranks
  tmp=tmp[-length(tmp)]
  mylength_diar=mylength_diar-1
}
cat('DONE\n')

# Effective Duration of the alert:
mylength_diar=3-1+length(percentile_diar_alerte[alert_status=="alert" & name==k & code %in% mycode[(pos_j):(pos_j+mylength_diar-1)],alert_status])
#