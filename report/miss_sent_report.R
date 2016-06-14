#setwd("/media/herimanitra/Document/IPM_sentinelle/sentinel_hrmntr 291115/Sentinel")
###############################Missing sent report ############################
library(ReporteRs);require(Hmisc)

doc <- docx() 
path1="/media/herimanitra/Document/IPM_sentinelle/sentinel_hrmntr 291115/Sentinel/report/logo.png"
path2="/srv/shiny-server/sentinel_hrmntr/Sentinel/report/logo.png"
mylogo=ifelse(file.exists(path1),path1,path2)
doc=addImage(doc, mylogo,
             par.properties = parProperties(text.align = "left", padding = 5), 
             width=5,
             height=1.5)
# Change the default font size and font family
options('ReporteRs-fontsize'=10, 'ReporteRs-default-font'='Arial')
# Add a formatted paragraph of texts:
doc = addTitle(doc, "Sms hebdomadaire du réseau sentinelle", level=1)
doc = addParagraph(doc , pot( Sys.Date(), textItalic() ) )
doc = addParagraph(doc, "        ")
baseCellProp = cellProperties( padding = 2 )
vertical_text= cellProperties(text.direction = "btlr",
                              padding = 2)
horizontal_text= cellProperties(text.direction = "lrtb",
                              padding = 2)

cat("load data and preprocess missing sent...")
require(data.table)
path1="/media/herimanitra/Document/IPM_sentinelle/sentinel_hrmntr 291115/Sentinel/data/missing_sent.csv"
path2="/srv/shiny-server/sentinel_hrmntr/Sentinel/data/missing_sent.csv"
myfile=ifelse(file.exists(path1),path1,path2)
missing_sent= fread(myfile)
missing_sent[,code:=paste0(substr(Annee,3,4),"/",ifelse(nchar(Semaine)<2,paste0("0",Semaine),Semaine))]
cat("DONE\n")

cat("crosstabulation of weeks and Sites...")
missing_sent[,Centre:=capitalize(tolower(Centre))]
X=table(missing_sent$code,missing_sent$Centre)
cat("DONE\n")

cat("Add Semaine épidémiologique...")
X=cbind(Semaine=row.names(X),X)
cat("DONE\n")

cat("load sentinel lat/long define 34sites vs other...")
path1="/media/herimanitra/Document/IPM_sentinelle/sentinel_hrmntr 291115/Sentinel/data/sentinel.csv"
path2="/srv/shiny-server/sentinel_hrmntr/Sentinel/data/sentinel.csv"
myfile=ifelse(file.exists(path1),path1,path2)
sentinel_latlong = fread(myfile)
sites34 = which( tolower(colnames(X)) %in% tolower(c("CSBU MANANJARY MNJ",
                                    "TSIMADILO",
                                    sentinel_latlong$centre) ))

other_site =which( !( tolower(colnames(X)) %in% tolower(c("CSBU MANANJARY MNJ",
                                        "TSIMADILO",
                                        sentinel_latlong$centre)) ))
cat("DONE\n")

cat("divide data into 03 parts...")
#X=table(missing_sent$code,missing_sent$Centre)
X1=X[,c(1,sites34[1:15])]
X2=X[,c(1,sites34[16:33])]
#select only period where data collection has begun
X3=X[95:nrow(X),other_site[1:10]]
X4=X[95:nrow(X),c(1,other_site[11:length(other_site)])]
cat("DONE\n")

#save 20 sites in HTC:
# missing_sent[,Centre2:=tolower(Centre2)]
# write.table(unique(missing_sent[Centre %in% colnames(X)[other_site[-1]],list(Centre,Centre2)]),
#              "report/site20.csv",row.names=F,sep=";")

#vertical test for headers
mytable1 = vanilla.table(X1,text.direction = "btlr")
mytable2 = vanilla.table(X2,text.direction = "btlr")
mytable3 = vanilla.table(X3,text.direction = "btlr")
mytable4 = vanilla.table(X4,text.direction = "btlr")


#loop through columns and change into red those cells with value <4:
#First line in vertical order
for ( k in 2:ncol(X1) ) 
{
  cat(k,"\n")
  mytable1[as.numeric(X1[,k]) >= 4, k] =  textProperties( font.size = 10)
  mytable1[as.numeric(X1[,k]) < 4, k] =  textProperties( color="#FF3333",font.size = 10)
}
for ( k in 2:ncol(X2) ) 
{
  cat(k,"\n")
  mytable2[as.numeric(X2[,k]) >= 4, k] =  textProperties( font.size = 10)
  mytable2[as.numeric(X2[,k]) < 4, k] =  textProperties( color="#FF3333",font.size = 10)
}
for ( k in 2:ncol(X3) ) 
{
  cat(k,"\n")
  mytable3[as.numeric(X3[,k]) >= 4, k] =  textProperties( font.size = 10)
  mytable3[as.numeric(X3[,k]) < 4, k] =  textProperties( color="#FF3333",font.size = 10)
}
for ( k in 2:ncol(X4) ) 
{
  cat(k,"\n")
  mytable4[as.numeric(X4[,k]) >= 4, k] =  textProperties( font.size = 10)
  mytable4[as.numeric(X4[,k]) < 4, k] =  textProperties( color="#FF3333",font.size = 10)
}
cat("Writing document to a word document...")
mytable1 = addFooterRow( mytable1, 
                        value = c("En rouge , les cas <4"),
                        cell.properties = horizontal_text
                        ,colspan = ncol(X1))
mytable2 = addFooterRow( mytable2, 
                         value = c("En rouge , les cas <4"),
                         cell.properties = horizontal_text
                         ,colspan = ncol(X2))
mytable3 = addFooterRow( mytable3, 
                         value = c("En rouge , les cas <4"),
                         cell.properties = horizontal_text
                         ,colspan = ncol(X3))
mytable4 = addFooterRow( mytable4, 
                         value = c("En rouge , les cas <4"),
                         cell.properties = horizontal_text
                         ,colspan = ncol(X4))


#
doc = addFlexTable( doc, mytable1 )
doc = addParagraph(doc, "        ")
doc = addFlexTable( doc, mytable2 )
doc = addParagraph(doc, "        ")
doc = addFlexTable( doc, mytable3 )
doc = addParagraph(doc, "        ")
doc = addFlexTable( doc, mytable4 )

writeDoc(doc, file = "missing_sent.docx")
#sudo apt-get install unoconv
Sys.sleep(5)
system("doc2pdf missing_sent.docx") #write in pdf using cli command
cat('DONE\n')