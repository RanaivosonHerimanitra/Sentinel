###############################Missing sent report ############################
library(ReporteRs)

doc <- docx() 

# Change the default font size and font family
options('ReporteRs-fontsize'=11, 'ReporteRs-default-font'='Arial')
# Add a formatted paragraph of texts
#++++++++++++++++++++++++++++++
doc = addTitle(doc, "Sms hebdomadaire du réseau sentinelle", level=1)
doc = addParagraph(doc , pot( Sys.Date(), textItalic() ) )
doc = addParagraph(doc, "        ")
baseCellProp = cellProperties( padding = 2 )
vertical_text= cellProperties(text.direction = "btlr",
                              padding = 2)

#load data and preprocessing:
require(data.table)
missing_sent= fread("missing_sent.csv")
missing_sent[,code:=paste0(Annee,"_",Semaine)]
#crosstab:
X=table(missing_sent$code,missing_sent$Centre)
X1=X[,1:27]
X2=X[,28:53]
mytable1 = FlexTable( X1, add.rownames = TRUE ,
                     header.cell.props = vertical_text
                     )
mytable1[]=textProperties(font.size = 10)
mytable2 = FlexTable( X2, add.rownames = TRUE ,
                      header.cell.props = vertical_text
)
mytable2[]=textProperties(font.size = 10)

#loop through columns and change into red those cells with value <4:
#First line in vertical order
for ( k in 1:ncol(X1) ) 
{
  cat(k,"\n")
  mytable1[X1[,k] < 4, k+1] =  textProperties( color="#FF3333")
}
for ( k in 1:ncol(X2) ) 
{
  cat(k,"\n")
  mytable2[X2[,k] < 4, k+1] =  textProperties( color="#FF3333")
}
cat("Writing document to a word document...")
mytable1 = addFooterRow( mytable1, 
                        value = c("En rouge , les cas <4")
                        ,colspan = ncol(X1)+1 )
mytable2 = addFooterRow( mytable2, 
                         value = c("En rouge , les cas <4")
                         ,colspan = ncol(X2)+1)

doc = addFlexTable( doc, mytable1 )
doc = addParagraph(doc, "        ")
doc = addFlexTable( doc, mytable2 )

writeDoc(doc, file = "missing_sent.docx")
#sudo apt-get install unoconv
system("doc2pdf missing_sent.docx") #write in pdf using cli command
cat('DONE\n')