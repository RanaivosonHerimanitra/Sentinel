################################Append narration title depending on sites and diseases########

###########if disease is Palu then we have 03 possibilities.

################First, if We are in Tana ville 

#########################Second, if We are in Tana périphérie

###############################For all except the 02 above


########### if disease is not Palu then no matter the sites are


#####################################################################################
title_yes=F
if (mylength_NA>0)
{
  doc <- addParagraph(doc,  pot(paste("-  ",k,":"),en_gras(fontsize = 12)) )
  title_yes=T
}
if ( L_palu )
{
 
  #######"""fIRST BLOCK  ########
  if (L_tana==F & L_hautplateau==F)
  {
    cat('because we are not in Tana or périphérie...\n')
    doc <- addParagraph(doc,  pot(paste("-  ",k,":"),en_gras(fontsize = 12)) )
    title_yes=T
  }
  #######"""2nd BLOCK  ########
  if ( L_tana & Nbcases>0 )
  {
      cat('because we are in Tsaralalana, Behoririka,Andohatapenaka...\n')
      doc <- addParagraph(doc,  pot(paste("-   ",k,":"),en_gras(fontsize = 12)) )
      title_yes=T
  } 
  #######"""3rd BLOCK  ########
  if (L_hautplateau & Nbcases>0)
  {
      cat('because we are in Antsirabe,Fianarantsoa or Anjozorobe...\n')
      doc <- addParagraph(doc,  pot(paste("-  ",k,":"),en_gras(fontsize = 12)) )
      title_yes=T
  } 
}
if ( L_diar & title_yes==F ) {
  doc <- addParagraph(doc,  pot(paste("-  ",k,":"),en_gras(fontsize = 12)) )
  title_yes=T
}
if ( L_tdr & title_yes==F ) {
  doc <- addParagraph(doc,  pot(paste("-  ",k,":"),en_gras(fontsize = 12)) )
  title_yes=T
}