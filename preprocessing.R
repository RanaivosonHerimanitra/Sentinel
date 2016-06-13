##################################Preprocessing for reporting purpose ###################

preprocessing_disease = function (select_htc=FALSE,include_all_sites=F)
{
  #initialize a list:
  data_list=list()
  source("diseases_control.R")
  #--ensure that list here are the same as in source("diseases_control.R")
  for ( j in c("Malaria","Diarrhea","Diarrhée fébrile","ILI","PFA") )
  {
    #cat("Disease:",j,"\n")
    data=select_disease(disease=j)
    
    #cat("convert data table to data.table format...")
    #data=as.data.table(as.data.frame(data))
    
    #sites for the display
    include<-c("deb_sem","code","abv","abz","ahh","ajb","atb","bel","bhk","boe","bos",
               "cda","die","dri","ejd","far","fns","iho","mae","mdv","mhj","mia","mjr",
               "mnj","mrb","mrg","mrt","mtn","nsb","sbv","stm","tdd","tgr","tlr","toa","tsl")
    #HTC: Hautes Terres Centrales:
    htc= c("deb_sem","code","abon","ambl","ants","bela","diri","dona","fito","mand","miad","mnld",
           "mora","samp","snam","tanb","tomp","tsar","tsim","velo","vink","vola")
    htc.name =c("abon","Ambatolahy","Antsampandrano",
                "Belanitra","diri","dona","fito","Manandona",
                "miad","mnld","Morarano","samp","Sabotsy Namehana",
                "Antanambao","tomp","tsar","tsim","velo","Vinanikarena","vola")
    #sites by faciès:
    East<-c("far","sbv","mnj","mrt","stm","toa","tgr")
    South<-c("abv","ejd")
    High_land<-c("cda","atb","bos","tsl","mjr","bhk","fns","ajb")
    Fringe<-c("ajb","iho","tdd","abz","mrg")
    excepted_East<- c("abv","abz","ahh","ajb","atb","bel","bhk","boe","bos",
                      "cda","die",
                      "dri","ejd","fns","iho","mae","mdv","mhj","mia","mjr",
                      "mrb","mrg","mtn",
                      "nsb","tdd","tlr","tsl")
    excepted_High_land<-c("abv","abz","ahh","bel","boe","die","dri","ejd","far","iho",
                          "mae","mdv","mhj","mia","mnj","mrb","mrg","mrt","mtn","nsb","sbv",
                          "stm","tdd",
                          "tgr","tlr","toa")
    ####################################data preprocessing#############
    
    #cat('DONE\n')
    #cat("keep only sites that already have historical values...\n")
    if (select_htc==FALSE)
    {
      if ( j!="ILI" & include_all_sites==F)
      {
       
        data=data[,include,with=F]
      }
      if ( j!="ILI" & include_all_sites==T)
      {
        
        data=data[,unique(c(include,htc)),with=F]
      }
     
    } else {
      data=data[,htc,with=F]
    }
    
    #cat('reshape data...')
    data=as.data.table(gather(data,key=sites,value=occurence,-c(code,deb_sem)))
    #cat('DONE\n')
    
    source("create_facies.R")
    data=create_facies(data)
    
    #cat('merge with different facies...')
    
    data[sites %in% East,East:=1]
    data[sites %in% South,South:=1]
    data[sites %in% High_land,High_land:=1]
    data[sites %in% Fringe,Fringe:=1]
    data[sites %in% excepted_East,excepted_East:=1]
    data[sites %in% excepted_High_land,excepted_High_land:=1]
    #cat('DONE\n')
    
    
    
    
    #cat('Extract weeks and years from data...')
    data[,weeks:=as.numeric(substr(code,6,8))]
    data[,years:=as.numeric(substr(code,1,4))]
    #cat('DONE\n')
    #cat("convert site to character...")
    data[,sites:=as.character(sites)]
    setkeyv(data,c('sites','weeks','years','code'))
    #cat("DONE\n") 
    data_list[[j]]=data
  
  }
  
  return(data_list)
}