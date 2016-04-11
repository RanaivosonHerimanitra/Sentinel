source("import_data.R",local = T)
source("prepare_data_forecast.R",local = T)
source("forecasting_functions.R",local = T)
############################### run model (update forecast) #################
alpha_range= seq(0.1,0.9,0.05)
beta_range= seq (0.1,0.9,0.05)
mymae=1e3
for ( a in alpha_range )
{
  for ( b in beta_range )
  {
    #store min of MAE  with parameters
    output = run_back_test(alpha=a,beta=b,plot=F,direction=direction)$mymae
    if (output<=mymae)
    {
      mymae = output
      best_a = a
      best_b = b
    } 
    cat("for",a , b , ":", output,"\n" )
  }
}

#retrieve preds and equation of the best model:
print (paste("best alpha",best_a))
print (paste("best beta",best_b))
model= run_back_test(alpha=best_a,beta=best_b,plot=F,direction=direction)

#handle <0:
preds=abs(model$preds) #more accurate?
#preds= ifelse(preds<0,0,preds)
fit1= model$mymodel
################################### save model and forecasts ###########################
  if (direction=="prospective")
  {
    save(fit1,preds,file="holt_prospective.rda")
    L_preds= length(preds)
    L=length(X$occurence)
    print (mae(X$occurence[(L-L_preds+1):L],preds))
    
    ts.plot(X$occurence)
    lines(round(c(X$occurence[1:(L-L_preds)],preds)),col="blue")
   
  } else {
    save(fit1,preds,file="holt_retrospective.rda")
    L_preds= length(preds)
    L=length(X$occurence)
    print (mae(X$occurence[1:L_preds],preds))
    
    ts.plot(X$occurence)
    lines(c(preds,X$occurence[(L_preds+1):L]),col="blue")
    
  }
#######################################################################################
