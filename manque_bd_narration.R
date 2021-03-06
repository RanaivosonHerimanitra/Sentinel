############################Script to detect NA and duration in database################
mylength_NA=0
#retrieve corresponding code to name (k):
k_code=sentinel_latlong[name==k,get("sites")]
Z1= PaluConf[code==mycode[pos_j],get(k_code)]
Z2= Diarrh[code==mycode[pos_j],get(k_code)]

# if belongs to these sites prior to 2015_29 then do nothing
group1=any(k_code %in% c("ambl","abon","tanb","diri","dona","samp","snam","bela","mand","mnld","velo","vink","tsar","mora","tsim","miad","vola","tomp")) & which(mycode==j)>=which(mycode=="2015_29")
# if belongs to "ANDRANOFITO" site prior to 2015_08 then do nothing
group2= any(k_code %in% "fito" ) & which(mycode==j)>=which(mycode=="2015_09")
if ( group1 | group2 ) {
  cat("do nothing because data collection from ",k," site did not begin at ",j,"\n")
} else {
  #count duration (length ) of lack of DB
  h=0
  while ( all(is.na(Z1)) & all(is.na(Z2)) & (pos_j+h)<=length(mycode) )
  {
    mylength_NA = mylength_NA +1
    h=h+1
    Z1= PaluConf[code %in% mycode[pos_j:(pos_j+h)] ,get(k_code)]
    Z2= Diarrh[code %in% mycode[pos_j:(pos_j+h)] ,get(k_code)]
  }
  
}
