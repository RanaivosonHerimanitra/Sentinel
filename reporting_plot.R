##################Source code for plotting Malaria TDR + and Fiever######
generate_plot=function(htc="all",
                       mydata=PaluConf_tdr,
                       disease1=NULL,
                       disease2=NULL,
                       disease1.targetvar="malaria_cases",
                       disease2.targetvar="SyndF",
                       legend.disease1="Paludisme TDR+",
                       legend.disease2="Fièvres",
                       title.label=NULL,
                       title.label.list=NULL,
                       title.ylab=NULL)
{
  if (htc=="all")
  {
    #extract years and select obs starting from 2009:
    mydata[,years:=as.numeric(substr(code,1,4))]
    mydata=mydata[years>=2009]
    
    #
    disease1= mydata[,c("code","deb_sem",disease1.targetvar),with=F]
    setnames(disease1,disease1.targetvar,"value")
    disease1[,Légende:=legend.disease1]
    disease2= mydata[,c("code","deb_sem",disease2.targetvar),with=F]
    setnames(disease2,disease2.targetvar,"value")
    disease2[,Légende:=legend.disease2]
    
    #sum per week (aggregation)
    disease1[,value:=sum(value,na.rm = T),by="code"]
    disease2[,value:=sum(value,na.rm = T),by="code"]
    
    
    X=rbind(unique(disease1), unique(disease2) )
    X$deb_sem=as.Date(X$deb_sem)
    X[,weeks:=week(deb_sem)]
    setnames(X,"deb_sem","Date")
   
      d= ggplot(data=X,
                aes(x=Date,y=value,fill=Légende,colour=Légende)) 
      d=d + geom_line()  #alpha=0.6
      d= d + scale_color_manual(values=c("#CC6666", "#9999CC"))
      debut_annee=as.numeric(unique(X[weeks==1,Date]))
      d= d + geom_vline(xintercept = debut_annee,
                        linetype=4,colour="orange")
      d=d + ggtitle(label=title.label)
      d=d + xlab("Date(ligne orange=1er janvier)") + ylab(title.ylab)
      return(d)
  } else {
    
    
    L=length(unique(mydata$name))
    myname = unique(mydata$name)
    myplot=list()
    for ( p in 1:L )
    {
      disease1= mydata[name==myname[p],c("code","deb_sem",disease1.targetvar),with=F]
      setnames(disease1,disease1.targetvar,"value")
      disease1[,Légende:=legend.disease1]
      disease2= mydata[name==myname[p],c("code","deb_sem",disease2.targetvar),with=F]
      setnames(disease2,disease2.targetvar,"value")
      disease2[,Légende:=legend.disease2]
      X=rbind(disease1, disease2)
      X$deb_sem=as.Date(X$deb_sem)
      X[,weeks:=week(deb_sem)]
      setnames(X,"deb_sem","Date")
      d= ggplot(data=X,
                aes(x=Date,y=value,fill=Légende,colour=Légende)) 
      d=d + geom_line() #alpha=0.6
      d= d + scale_color_manual(values=c("#CC6666", "#9999CC"))
      debut_annee=as.numeric(unique(X[weeks==1,Date]))
      d= d + geom_vline(xintercept = debut_annee,
                        linetype=4,colour="orange")
      d=d + ggtitle(label=paste(title.label.list, myname[p]))
      d=d + xlab("Date (ligne orange=1er janvier)") + ylab(title.ylab)
      myplot[[p]]=d
     print(myplot[[p]])
    }
  }
  
}