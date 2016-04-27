#############################Duration length for NA in lack of TDR ###################

#initialize duration of lack to 0
mylength_tdr=0
X=PaluConf_tdr[code==mycode[pos_j] & name==k,get("manque_tdr")]

#get length of X,which corresponds to duration of alert (lack of RDT kit)
#annee_duration = PaluConf_tdr[code==mycode[pos_j],get("Annee")]
#semaine_duration= PaluConf_tdr[code==mycode[pos_j],get("Semaine")]

#mylength_tdr=nrow(PaluConf_tdr[name==k & Annee<=annee_duration & Semaine<=semaine_duration])
d=0
while ( length(X)>0 & (pos_j+d)<=length(mycode) )
{
    mylength_tdr = mylength_tdr +1
    d=d+1
    X=PaluConf_tdr[code %in% mycode[pos_j+d] & name==k,get("manque_tdr")]

}

