select_disease = function(disease="Malaria")
{
  
  if ( disease=="Malaria") 
  {
    #cat("You have chosen to display",disease,"\n")
    return(PaluConf)
  }
  if ( disease=="Diarrhea") 
  {
    #cat("You have chosen to display",disease,"\n")
    return(Diarrh)
  }
  if ( disease=="Diarrhée fébrile")
  {
    #cat("You have chosen to display",disease,"\n")
    return(Diarrh_feb)
  }
  if ( disease=="ILI")
  {
    #cat("You have chosen to display",disease,"\n")
    #ILI = GrippSusp + AutrVirResp = Syndrome Grippal
    #Dengue-like = ArboSusp
    return(ili)
  }
  if ( disease=="PFA")
  {
    #cat("You have chosen to display",disease,"\n")
    return(pfa)
  }
}
