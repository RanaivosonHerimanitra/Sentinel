#######################################################################
run_model = function (serie, parameters=list(method=c("holt","ARIMAX"),
                                             direction=c("prospective","retrospective")),
                      begin=1,horizon=1,plot=T, l=round(nrow(X)/4),alpha=0.8,beta=0.2, metric=mae,save_model=T)
{   
  if ( parameters[["direction"]]=="prospective" )
  {
    #training set:
    tr_set= serie[begin:(begin+l)]
    #validation set:
    valid_set = serie[begin+horizon]
    #training begins:
    if (parameters[["method"]]=="holt")
    {
      fit1 <- holt(tr_set, alpha=alpha, beta=beta, initial="simple", h=horizon)
    }    
    #preprocess prediction into numeric format
    preds = forecast(fit1,h=horizon)
    preds = as.numeric(preds$mean)
    #if user want to plot (pass plot=T)
    if (plot==T)
    {
      #plot forecast
      ts.plot(serie)
      lines(c(tr_set,preds),col="blue")   
    }
  } else {
    #training set:
    tr_set = serie[(begin-l):begin]
    #validation set:
    valid_set = serie[begin+horizon]
    #training begins:
    if (parameters[["method"]]=="holt")
    {
      fit1 <- holt(tr_set, alpha=alpha, beta=beta, initial="simple", h=horizon)
    }    
    #preprocess prediction into numeric format
    preds = forecast(fit1,h=horizon)
    preds = as.numeric(preds$mean)
    #if user want to plot (pass plot=T)
    if (plot==T)
    {
      #plot forecast
      ts.plot(serie)
      lines(c(serie[1:(begin-l-1)],fitted(fit1),preds),col="blue")   
    }
  }
  
 
  #return MAE and predictions:
  return  (list(mymodel=fit1, mymae=mae(valid_set,preds),preds=preds ))
}
#########################################################################################################
run_back_test = function(alpha=0.8,beta=0.2,plot=F,direction="retrospective")
{
  counter=1;mymae=array();preds=array()
  if (direction=="retrospective")
  {
    for ( j in (nrow(X)-1):(round(nrow(X)/4)+1) )
    {
      result= run_model(serie=X$occurence, begin=j, horizon=1,plot=plot,
                        parameters=list(method="holt",direction=direction),
                        l=round(nrow(X)/4), alpha=alpha,beta=beta, 
                        metric=mae )
      #store MAE in an array
      mymae[counter]=result$mymae 
      #store predictions in an array
      preds[counter]= result$preds
      counter = counter + 1 
    }
    return (list( mymae=mean(mymae), preds=rev(preds)))
  } 
  if (direction=="prospective") 
  {
    for ( j in  1:(nrow(X)-round(nrow(X)/4)-1) )
    {
      result= run_model(serie=X$occurence, begin=j, horizon=1,plot=plot,
                        parameters=list(method="holt",direction=direction),
                        l=round(nrow(X)/4), alpha=alpha,beta=beta, 
                        metric=mae )
      #store MAE in an array
      mymae[counter]=result$mymae 
      #store predictions in an array
      preds[counter]= result$preds
      counter = counter + 1 
    }   
    return (list( mymae=mean(mymae), preds=(preds)))
   }
  
}


