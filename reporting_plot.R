##################Source code for plotting Malaria TDR + and Fiever######
generate_plot=function(htc="all",mydata=PaluConf_tdr,legend.disease1=NULL,
                       legend.disease2=NULL,
                       title.label=NULL)
{
  if (htc=="all")
  {
    #extract years and select obs starting from 2009:
    mydata[,years:=as.numeric(substr(code,1,4))]
    mydata=mydata[years>=2009]
    
    #melting:
    malaria= mydata[,list(code,deb_sem,malaria_cases)]
    setnames(malaria,"malaria_cases","value")
    malaria[,Légende:=legend.disease1]
    fievre= mydata[,list(code,deb_sem,SyndF)]
    setnames(fievre,"SyndF","value")
    fievre[,Légende:=legend.disease2]
    
    X=rbind(malaria, fievre)
    X$deb_sem=as.Date(X$deb_sem)
    X[,weeks:=week(deb_sem)]
    setnames(X,"deb_sem","Date")
   
      d= ggplot(data=X,
                aes(x=Date,y=value,fill=Légende,colour=Légende)) 
      d=d + geom_line(alpha=0.6)
      debut_annee=as.numeric(unique(X[weeks==1,Date]))
      d= d + geom_vline(xintercept = debut_annee,
                        linetype=4,colour="purple")
      d=d + ggtitle(label="Fièvres et Paludisme dans le réseau sentinel")
      d=d + xlab("Date(ligne violette=1er janvier)") + ylab("Cas de Fièvres et Paludisme")
      return(d)
  } else {
    
    
    L=length(unique(mydata$name))
    myname = unique(mydata$name)
    
    #par(mfrow=c(1,2))
    myplot=list()
    for ( p in 1:L )
    {
      malaria= mydata[name==myname[p],list(code,deb_sem,malaria_cases)]
      setnames(malaria,"malaria_cases","value")
      malaria[,Légende:="Paludisme TDR+"]
      fievre= mydata[,list(code,deb_sem,SyndF)]
      setnames(fievre,"SyndF","value")
      fievre[,Légende:="Fièvres"]
      
      X=rbind(malaria, fievre)
      X$deb_sem=as.Date(X$deb_sem)
      X[,weeks:=week(deb_sem)]
      setnames(X,"deb_sem","Date")
      d= ggplot(data=X,
                aes(x=Date,y=value,fill=Légende,colour=Légende)) 
      d=d + geom_line(alpha=0.6)
      debut_annee=as.numeric(unique(X[weeks==1,Date]))
      d= d + geom_vline(xintercept = debut_annee,
                        linetype=4,colour="purple")
      d=d + ggtitle(label=paste("Fièvres et Paludisme à", myname[p]))
      d=d + xlab("Date (ligne violette=1er janvier)") + ylab("Cas de Fièvres et Paludisme")
      myplot[[p]]=d
     print(myplot[[p]])
    }
  }
  
}