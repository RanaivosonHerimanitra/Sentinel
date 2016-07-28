##############################Prepare data for forecasting ####################
require(forecast)
mae = function (actual,preds)
{
  return ( mean(abs(actual-preds)) )
}
#only select far and mjr
X=PaluConf[,list(code,deb_sem,far,mjr)]
#training length:
#training_length=round(nrow(X)/4)
#spread data
prepare_load= function(mymodel=NULL)
{
  X=as.data.table(gather(PaluConf,key=sites,value=occurence,-c(code,deb_sem)))
  setorder(X,deb_sem)
  # Farafangana & Mananjary
  X=X[sites %in% c("far","mjr")]
  #sum per month , per year: 
  X[,mymonth:=month(as.Date(deb_sem))]
  X[,myyear:=year(as.Date(deb_sem))]
  X[,occurence:=sum(occurence,na.rm = T),by="mymonth,myyear"]
  
  if ( mymodel=="ARMAX" )
  {
   
    X=unique(X[,list(code,deb_sem,occurence,mymonth,myyear)])
    min_semaine=min(X$deb_sem)
    max_semaine=max(X$deb_sem)
    lg = function (x) c( NA, x[-length(x)])
    
    cat("merge with LST...")
    lst = hfi[type_val=="LST_DAY"];
    X=merge(X,lst[deb_sem>=min_semaine,list(code,far,mjr)],
            by.x="code",by.y="code",all.x = T)
    setnames(X,old=c("far","mjr"),new=c("lst_far","lst_mjr"))
    cat("DONE\n")
   
  cat("building lags for temperature (LST)...") 
    for ( j in 1:8 )
    {
      if ( length(grep("lst_far_lag|lst_mjr_lag",names(X),value = T))==0 )
      {
        X[,paste0("lst_far_lag",j):=lg(lst_far)]
        X[,paste0("lst_mjr_lag",j):=lg(lst_mjr)]
        #remove unused var for memory
        X[,lst_far:=NULL]
        X[,lst_mjr:=NULL]
      } else {
        X[,paste0("lst_far_lag",j):=lg(get(paste0("lst_far_lag",j-1)))]
        X[,paste0("lst_mjr_lag",j):=lg(get(paste0("lst_mjr_lag",j-1)))]
      }
    }
  cat("DONE\n")
    
    
    cat("merge with Rainfall...")
    pmm = hfi[type_val=="PRECIPITATION"]
    pmm_tmp= pmm[,list(mean(far,na.rm = T),mean(mjr,na.rm = T)),by="code"]
    setnames(pmm_tmp,"V1","pmm_far");setnames(pmm_tmp,"V2","pmm_mjr")
    pmm_tmp=unique(pmm_tmp[,list(code,pmm_far,pmm_mjr)])
    X=merge(X,pmm_tmp[,list(code,pmm_far,pmm_mjr)],
            by.x="code",by.y="code",all.x = T)
    cat("DONE\n")
    
    cat("building lags for precipitation (pmm)...") 
    for ( j in 1:8 )
    {
      if ( length(grep("pmm_far_lag|pmm_mjr_lag",names(X),value = T))==0 )
      {
        X[,paste0("pmm_far_lag",j):=lg(pmm_far)]
        X[,paste0("pmm_mjr_lag",j):=lg(pmm_mjr)]
        #remove unused var for memory
        X[,pmm_far:=NULL]
        X[,pmm_mjr:=NULL]
      } else {
        X[,paste0("pmm_far_lag",j):=lg(get(paste0("pmm_far_lag",j-1)))]
        X[,paste0("pmm_mjr_lag",j):=lg(get(paste0("pmm_mjr_lag",j-1)))]
      }
    }
    cat("DONE\n")
    
    cat("merge with ndvi...")
    ndvi = hfi[type_val=="NDVI"];
    X=merge(X,ndvi[deb_sem>=min_semaine,list(code,far,mjr)],
            by.x="code",by.y="code",all.x = T)
    setnames(X,old=c("far","mjr"),new=c("ndvi_far","ndvi_mjr"))
    cat("DONE\n")
    
    cat("building lags for NDVI...") 
    for ( j in 1:8 )
    {
      if ( length(grep("ndvi_far_lag|ndvi_mjr_lag",names(X),value = T))==0 )
      {
        X[,paste0("ndvi_far_lag",j):=lg(ndvi_far)]
        X[,paste0("ndvi_mjr_lag",j):=lg(ndvi_mjr)]
        #remove unused var for memory
        X[,ndvi_far:=NULL]
        X[,ndvi_mjr:=NULL]
      } else {
        X[,paste0("ndvi_far_lag",j):=lg(get(paste0("ndvi_far_lag",j-1)))]
        X[,paste0("ndvi_mjr_lag",j):=lg(get(paste0("ndvi_mjr_lag",j-1)))]
      }
    }
    cat("DONE\n")
    
    
    cat("merge with IRS(CAID)...")
    caid=fread("data/caid.csv")
    caid[,code:=paste0(substr(deb_sem,1,4),"_",substr(deb_sem,6,7))]
    caid_tmp= caid[,list(sum(far,na.rm = T),sum(mjr,na.rm = T)),by="code"]
    setnames(caid_tmp,"V1","caid_far");setnames(caid_tmp,"V2","caid_mjr")
    caid_tmp=unique(caid_tmp[,list(code,caid_far,caid_mjr)])
    X=merge(X,caid_tmp[,list(code,caid_far,caid_mjr)],
            by.x="code",by.y="code",all.x = T)
    cat("DONE\n")
   
    
    
    cat("merge with LLIN/MILD...")
    mild=fread("data/mild_export.csv")
    mild[,code:=paste0(substr(deb_sem,1,4),"_",substr(deb_sem,6,7))]
    
    cat("DONE\n")
    
    #build time elapsed since last LLIN:
    for ( k in c("far","mjr") )
    {
      cat("build time elapsed since last LLIN for",k,"...")
      index_intervention=which(mild[,get(k)]==1)
      index_intervention_diff=index_intervention - lag(index_intervention)
      index_intervention_diff= which(index_intervention_diff>1)
      if ( length(index_intervention_diff)==0 )
      {
        last_index_intervention=index_intervention[length(index_intervention)] ;
        last_date_intervention=mild[last_index_intervention,get("deb_sem")]
        mild[,paste0("time_elapsed_LLIN_",k):=0]
        mild[last_index_intervention:nrow(mild),paste0("time_elapsed_LLIN_",k):=as.numeric(0:(nrow(mild)-last_index_intervention))]
      
      } else {
        last_index_intervention=index_intervention[index_intervention_diff] ;
        last_date_intervention=mild[last_index_intervention,get("deb_sem")]
        mild[,paste0("time_elapsed_LLIN_",k):=0]
        counter=1
       for ( s in (last_index_intervention[1]):nrow(mild) )
       {
         if ( !( as.character(s+1) %in% as.character(last_index_intervention) )   )
         {
           if (s+1 <=nrow(mild) )
               {
                 mild[s+1,paste0("time_elapsed_LLIN_",k):=counter]
                 counter=counter + 1 
               }
         } else {
           if (s+1 <=nrow(mild) )
           {
           counter=0
           mild[s+1,paste0("time_elapsed_LLIN_",k):=counter]
           counter=counter + 1 
           }
         }
       }
      }
      cat("DONE\n")
    }
   
    X=merge(X,mild[deb_sem>=min_semaine,list(code,time_elapsed_LLIN_far,time_elapsed_LLIN_mjr)],
             by.x="code",by.y="code",all.x = T)
   
    cat("reoder time series...")
    setorder(X,-deb_sem)
    cat("DONE\n")
    
   
  } else {
   
    #retrieve unique values:
    X=unique(X[,list(mymonth,myyear,occurence)])
    return (X)
  }
  
  
}
