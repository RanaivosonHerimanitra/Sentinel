require(Hmisc)

cat("load data and preprocess missing sent..")
require(data.table)
missing_sent= fread("/media/herimanitra/Document/IPM_sentinelle/sentinel_hrmntr 291115/Sentinel/data/missing_sent.csv")
missing_sent[,code:=paste0(Annee,"_",ifelse(nchar(Semaine)<2,paste0("0",Semaine),Semaine))]
cat("DONE\n")
cat("select only >=2016 obs...")
missing_sent=missing_sent[Annee>=2016]
cat("DONE\n")
cat("crosstabulation of weeks and Sites...")
missing_sent[,Centre:=capitalize(tolower(Centre))]
X=table(missing_sent$code,missing_sent$Centre)
cat("DONE\n")

cat("Add Semaine épidémiologique...")
X=cbind(Semaine=row.names(X),X)
cat("DONE\n")

cat("load sentinel lat/long define 34sites vs other...")
#sentinel_latlong = fread("/media/herimanitra/Document/IPM_sentinelle/sentinel_hrmntr 291115/Sentinel/data/sentinel.csv")
sites34 = which( tolower(colnames(X)) %in% tolower(c("CSBU MANANJARY MNJ",
                                                     "TSIMADILO",
                                                     sentinel_latlong$centre) ))

other_site =which( !( tolower(colnames(X)) %in% tolower(c("CSBU MANANJARY MNJ",
                                                          "TSIMADILO",
                                                          sentinel_latlong$centre)) ))
cat("DONE\n")

# save CSB to retrieve later:
#write.table(colnames(X)[-1],"report/CSB.csv",row.names = F,sep=";")