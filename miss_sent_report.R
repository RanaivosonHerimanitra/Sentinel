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
missing_sent= fread("data/missing_sent.csv")
missing_sent[,code:=paste0(substr(Annee,3,4),"/",Semaine)]
cat("DONE\n")

cat("crosstabulation...")
X=table(missing_sent$code,missing_sent$Centre)
cat("DONE\n")

cat("load sentinel lat/long define 34sites vs other...")
sentinel_latlong = fread("data/sentinel.csv")
sites34 = which( tolower(colnames(X)) %in% tolower(c("CSBU MANANJARY MNJ",
                                    "TSIMADILO",
                                    sentinel_latlong$centre) ))

other_site =which( !( tolower(colnames(X)) %in% tolower(c("CSBU MANANJARY MNJ",
                                        "TSIMADILO",
                                        sentinel_latlong$centre)) ))
cat("DONE\n")

cat("crosstabulation...")
X=table(missing_sent$code,missing_sent$Centre)
X1=X[,sites34]
X2=X[,other_site]
X2=X2[apply(X2,1,sum)>0,] #select only period where data collection has begun
cat("DONE\n")

#vertical text for headers
mytable1 = FlexTable(X1, add.rownames = TRUE ,
                     header.cell.props = vertical_text,
                     header.text.props = textProperties(font.weight='normal',font.size = 7),
                     body.text.props = textProperties(font.size = 5)
                     )

#vertical test for headers
mytable2 = FlexTable( X2, add.rownames = TRUE ,
                      header.cell.props = vertical_text,
                      header.text.props = textProperties(font.weight='normal',font.size = 7),
                      body.text.props = textProperties(font.size = 7)
)

#decrease font.weight of first column which corresponds to date:
mytable1[,1]=textProperties(font.size = 7)
mytable2[,1]=textProperties(font.size = 7)
# set column widths:
first_cell_width= 1.25
taille1 = ncol(X1)-1
ratio1 = (taille1 - 1.25)/taille1
taille2 = ncol(X2)-1
ratio2 = (taille2 - 1.25)/taille2

mytable1=setFlexTableWidths( mytable1, widths = c(first_cell_width, rep(ratio1, taille1) ))
mytable2=setFlexTableWidths( mytable2, widths = c(first_cell_width, rep(ratio2, taille2 ) ))
#
#loop through columns and change into red those cells with value <4:
#First line in vertical order
for ( k in 1:ncol(X1) ) 
{
  cat(k,"\n")
  mytable1[X1[,k] >= 4, k+1] =  textProperties( font.size = 6)
  mytable1[X1[,k] < 4, k+1] =  textProperties( color="#FF3333",font.size = 6)
}
for ( k in 1:ncol(X2) ) 
{
  cat(k,"\n")
  mytable2[X2[,k] >= 4, k+1] =  textProperties( font.size = 6)
  mytable2[X2[,k] < 4, k+1] =  textProperties( color="#FF3333",font.size = 6)
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

doc = addFlexTable( doc, mytable1 )
doc = addParagraph(doc, "        ")
doc = addFlexTable( doc, mytable2 )

writeDoc(doc, file = "missing_sent.docx")
#sudo apt-get install unoconv
#system("doc2pdf missing_sent.docx") #write in pdf using cli command
cat('DONE\n')