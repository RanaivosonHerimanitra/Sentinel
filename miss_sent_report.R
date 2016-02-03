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



#load data and preprocessing:
require(data.table)
missing_sent= fread("missing_sent.csv")
missing_sent[,code:=paste0(Annee,"_",Semaine)]
#crosstab:
X=table(missing_sent$Centre,missing_sent$code)

# loop with semaine epidemiologic of 4 length:
j=1
while ( j < ncol(X) )
{
  cat(j,"\n")
  if ( j+4 > ncol(X) )
  {
    #define table
    tmp = X[,j:ncol(X)]
    mytable = FlexTable( tmp , add.rownames = TRUE )
    #loop to attribute colors:
    for ( k in 1:ncol(tmp) ) {
      setFlexTableBackgroundColors( mytable, 
                                    j=k,
                                    colors = ifelse(as.numeric(tmp[,k])<3,"red","white"))
    }
    j=ncol(X)+1
  } else {
    tmp= X[,j:(j+4)]
    #define table
    mytable = FlexTable( tmp, add.rownames = TRUE )
    #loop to attribute colors
    for ( k in 1:ncol(tmp) ) {
      # set background colors:
      setFlexTableBackgroundColors( mytable, 
                                    j=k+1,
                                    colors = ifelse(as.numeric(tmp[,k])<3,"red","white"))
    }
    j=j+5
  }
  
  
  doc = addFlexTable( doc, mytable )
  doc = addParagraph(doc, "        ")
}


cat("Writing document to a word document...")
writeDoc(doc, file = "missing_sent.docx")
cat('DONE\n')