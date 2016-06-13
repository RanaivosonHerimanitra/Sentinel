############################Script to detect NA and duration in database################
mylength_NA=0
#retrieve corresponding code to name (k):
k_code=sentinel_latlong[name==k,get("sites")]
Z1= PaluConf[code==mycode[pos_j],get(k_code)]
Z2= Diarrh[code==mycode[pos_j],get(k_code)]


h=0
while ( all(is.na(Z1)) & all(is.na(Z2)) & (pos_j+h)<=length(mycode) )
{
  mylength_NA = mylength_NA +1
  h=h+1
  Z1= PaluConf[code %in% mycode[pos_j:(pos_j+h)] ,get(k_code)]
  Z2= Diarrh[code %in% mycode[pos_j:(pos_j+h)] ,get(k_code)]
}
