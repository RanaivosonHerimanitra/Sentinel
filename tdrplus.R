tdr_malaria = function (htc=FALSE)
{
  htc_code= c("deb_sem","code","abon","ambl","ants","bela",
         "diri","dona","fito","mand","miad","mnld",
         "mora","samp","snam","tanb","tomp","tsar",
         "tsim","velo","vink","vola")
  include<-c("deb_sem","code","abv","abz","ahh","ajb","atb","bel","bhk","boe","bos",
             "cda","die","dri","ejd","far","fns","iho","mae","mdv","mhj","mia","mjr",
             "mnj","mrb","mrg","mrt","mtn","nsb","sbv","stm","tdd","tgr","tlr","toa","tsl")
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
  PaluConf=as.data.table(as.data.frame(PaluConf))
  if (htc==F)
  {
    PaluConf=PaluConf[,include,with=F]
  } else {
    PaluConf=PaluConf[,htc_code,with=F]
  }
  
  PaluConf=as.data.table(gather(PaluConf,
                                key=sites,
                                value=malaria_cases,-c(code,deb_sem)))
  PaluConf_tdr=merge(PaluConf,tdr_eff,
                     by.x=c("code","sites"),
                     by.y=c("code","sites") )
 PaluConf_tdr[,manque_tdr:=ifelse(SyndF-TestPalu>0,1,0)]

 return(PaluConf_tdr)
  
}