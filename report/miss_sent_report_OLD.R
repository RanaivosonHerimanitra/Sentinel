#setwd("/media/herimanitra/Document/IPM_sentinelle/sentinel_hrmntr 291115/Sentinel")
###############################Missing sent report ############################
library(ReporteRs)

doc <- docx() 

# Change the default font size and font family
options('ReporteRs-fontsize'=10, 'ReporteRs-default-font'='Arial')
# Add a formatted paragraph of texts
#++++++++++++++++++++++++++++++
doc = addTitle(doc, "Sms hebdomadaire du réseau sentinelle", level=1)
doc = addParagraph(doc , pot( Sys.Date(), textItalic() ) )
doc = addParagraph(doc, "        ")
baseCellProp = cellProperties( padding = 2 )
vertical_text= cellProperties(text.direction = "btlr",
                              padding = 2)
horizontal_text= cellProperties(text.direction = "lrtb",
                              padding = 2)

cat("load data and preprocess missing sent..")
require(data.table)
missing_sent= fread("/media/herimanitra/Document/IPM_sentinelle/sentinel_hrmntr 291115/Sentinel/data/missing_sent.csv")
missing_sent[,code:=paste0(substr(Annee,3,4),"/",ifelse(nchar(Semaine)<2,paste0("0",Semaine),Semaine))]
cat("DONE\n")

cat("crosstabulation of weeks and Sites...")
X=table(missing_sent$code,missing_sent$Centre)
cat("DONE\n")

cat("load sentinel lat/long define 34sites vs other...")
sentinel_latlong = fread("/media/herimanitra/Document/IPM_sentinelle/sentinel_hrmntr 291115/Sentinel/data/sentinel.csv")
sites34 = which( tolower(colnames(X)) %in% tolower(c("CSBU MANANJARY MNJ",
                                    "TSIMADILO",
                                    sentinel_latlong$centre) ))

other_site =which( !( tolower(colnames(X)) %in% tolower(c("CSBU MANANJARY MNJ",
                                        "TSIMADILO",
                                        sentinel_latlong$centre)) ))
cat("DONE\n")

cat("divide data into 03 parts...")
#X=table(missing_sent$code,missing_sent$Centre)
X1=X[,sites34[1:15]]
X2=X[,sites34[16:33]]
X3=X[,other_site]
X3=X3[apply(X3,1,sum)>0,] #select only period where data collection has begun
cat("DONE\n")


#vertical text for headers
mytable1 = FlexTable(X1, add.rownames = TRUE ,
                     header.cell.props = vertical_text,
                     header.text.props = textProperties(font.weight='normal',font.size = 10),
                     body.text.props = textProperties(font.size = 10)
                     )

#vertical test for headers
mytable2 = FlexTable( X2, add.rownames = TRUE ,
                      header.cell.props = vertical_text,
                      header.text.props = textProperties(font.weight='normal',font.size = 10),
                      body.text.props = textProperties(font.size = 10)
)

#vertical test for headers
mytable3 = FlexTable( X3, add.rownames = TRUE ,
                      header.cell.props = vertical_text,
                      header.text.props = textProperties(font.weight='normal',font.size = 10),
                      body.text.props = textProperties(font.size = 10)
)

#decrease font.weight of first column which corresponds to date:
mytable1[,1]=textProperties(font.size = 10)
mytable2[,1]=textProperties(font.size = 10)
mytable3[,1]=textProperties(font.size = 10)
# set column widths:
first_cell_width= 1.25
taille1 = ncol(X1) #before ncol(X1)-1 (12h30, 03 mai 2016)
ratio1 = (taille1 - 1.25)/taille1
taille2 = ncol(X2) #before ncol(X2)-1 (12h30, 03 mai 2016)
ratio2 = (taille2 - 1.25)/taille2
taille3=ncol(X3)
ratio3= (taille3 - 1.25)/taille3
mytable1=setFlexTableWidths( mytable1, 
                             widths = c(first_cell_width, rep(ratio1, taille1) ))
mytable2=setFlexTableWidths( mytable2, 
                             widths = c(first_cell_width, rep(ratio2, taille2 ) ))
mytable3=setFlexTableWidths( mytable3, 
                             widths = c(first_cell_width, rep(ratio3, taille3 ) ))
#
#loop through columns and change into red those cells with value <4:
#First line in vertical order
for ( k in 1:ncol(X1) ) 
{
  cat(k,"\n")
  mytable1[X1[,k] >= 4, k+1] =  textProperties( font.size = 10)
  mytable1[X1[,k] < 4, k+1] =  textProperties( color="#FF3333",font.size = 10)
}
for ( k in 1:ncol(X2) ) 
{
  cat(k,"\n")
  mytable2[X2[,k] >= 4, k+1] =  textProperties( font.size = 10)
  mytable2[X2[,k] < 4, k+1] =  textProperties( color="#FF3333",font.size = 10)
}
for ( k in 1:ncol(X3) ) 
{
  cat(k,"\n")
  mytable3[X3[,k] >= 4, k+1] =  textProperties( font.size = 10)
  mytable3[X3[,k] < 4, k+1] =  textProperties( color="#FF3333",font.size = 10)
}
cat("Writing document to a word document...")
mytable1 = addFooterRow( mytable1, 
                        value = c("En rouge , les cas <4"),
                        cell.properties = horizontal_text
                        ,colspan = ncol(X1)+1 )
mytable2 = addFooterRow( mytable2, 
                         value = c("En rouge , les cas <4"),
                         cell.properties = horizontal_text
                         ,colspan = ncol(X2)+1)
mytable3 = addFooterRow( mytable3, 
                         value = c("En rouge , les cas <4"),
                         cell.properties = horizontal_text
                         ,colspan = ncol(X3)+1)
doc = addFlexTable( doc, mytable1 )
doc = addParagraph(doc, "        ")
doc = addFlexTable( doc, mytable2 )
doc = addParagraph(doc, "        ")
doc = addFlexTable( doc, mytable3 )

writeDoc(doc, file = "missing_sent.docx")
#sudo apt-get install unoconv
Sys.sleep(5)
system("doc2pdf missing_sent.docx") #write in pdf using cli command
cat('DONE\n')