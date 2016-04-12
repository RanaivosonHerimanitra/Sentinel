#####################Create a facies for a given data.table ####################
create_facies = function (data=PaluConf)
{
  East<-c("far","sbv","mnj","mrt","stm","toa","tgr")
  South<-c("abv","ejd")
  High_land<-c("cda","atb","bos","tsl","mjr","bhk","fns","ajb")
  Fringe<-c("ajb","iho","tdd","abz","mrg")
  excepted_East<- c("abv","abz","ahh","ajb","atb","bel","bhk","boe","bos","cda",
                    "die","dri","ejd","fns","iho","mae","mdv","mhj","mia","mjr",
                    "mrb","mrg","mtn","nsb","tdd","tlr","tsl")
                    
  excepted_High_land<-c("abv","abz","ahh","bel","boe","die","dri","ejd","far","iho",
                        "mae","mdv","mhj","mia","mnj","mrb","mrg","mrt","mtn","nsb",
                        "sbv","stm","tdd","tgr","tlr","toa")
  
  
  #facies are not disjoint:
  setkey(data,sites)
  data[sites %in% East,facies:="East"]
  data[sites %in% South,facies:="South"]
  data[sites %in% High_land,facies:="High_land"]
  data[sites %in% Fringe,facies:="Fringe"]
  data[sites %in% excepted_East,facies:="excepted_East"]
  data[sites %in% excepted_High_land,facies:="excepted_High_land"]
 
  return(data)
}