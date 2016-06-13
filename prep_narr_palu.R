###########################Script for preparing narration for Palu ##############
dist= percentile_palu_alerte[name==k & is.na(occurence)==F,get("occurence")]

cat("calculate percentile rank for PALU...")
myranks_palu=array()
p=1-1
while ( (pos_j+p)<=length(mycode) & p!=11-1 )
{
  myranks_palu[p+1]=perc_rank( dist,
                             percentile_palu_alerte[code==mycode[pos_j+p] & name==k,get("occurence")] )
  p=p+1
}
cat("DONE\n")
cat('Handle alerts duration > 10 weeks for Palu...')
myranks_palu_additionnal=array(); myindex=1
alpha=11-1
if (  (pos_j+alpha)<length(mycode) )
{
  tmp= perc_rank(dist, percentile_palu_alerte[code==mycode[pos_j+alpha] & name==k,get("occurence")])
  tmp= ifelse (is.na(tmp)== T || length(tmp)==0,0,tmp)
  while ( tmp>=90 & (pos_j+alpha)<length(mycode) )
  {
    cat(k,'has reached the threshold at',alpha,'th week','with a rank:',tmp,'\n')
    myranks_palu_additionnal[myindex]=tmp
    myindex = myindex + 1
    alpha=alpha +1
    tmp= perc_rank(dist, percentile_palu_alerte[code==mycode[pos_j+alpha] & name==k,get("occurence")])
    tmp= ifelse (is.na(tmp)== T || length(tmp)==0,0,tmp)
  }
}
cat('DONE\n')

if ( length(myranks_palu_additionnal)>1)
{
  cat("Append alert beyond 11th week to first 10 weeks...")
  myranks_palu=c(myranks_palu,myranks_palu_additionnal)
  cat("DONE\n")
}

tmp=myranks_palu
tmp[is.na(tmp)==T]=0
mylength_palu=length(myranks_palu)
while( all(tmp>=90) == F & length(tmp)!=3  )
{
  #remove last value of myranks
  tmp=tmp[-length(tmp)]
  mylength_palu=mylength_palu-1
}

# Effective Duration of the alert:
mylength_palu=1-1+length(percentile_palu_alerte[alert_status=="alert" & name==k & code %in% mycode[(pos_j):(pos_j+mylength_palu-1)],alert_status])
#