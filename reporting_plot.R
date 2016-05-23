##################Source code for plotting Malaria TDR + and Fiever######
generate_plot=function(htc="all",
                       add_trend=FALSE,
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
    X$deb_sem=as.Date(X$deb_sem,origin="1970-01-01")
    X[,weeks:=week(deb_sem)]
    setnames(X,"deb_sem","Date")
   
      d= ggplot(data=X,
                aes(x=Date,y=value,fill=Légende)) 
      
      d=d + geom_bar(stat = "identity")  
      #if user specifies a trend:
      if (add_trend==T) {  d= d + geom_smooth(method='lm',formula=y~x) }
      #more vibrant color:
      d= d + scale_fill_manual(values=c("#0000ff", "#840000"))
      #increase space between legend keys:
      d= d + theme(legend.position="bottom", legend.direction="horizontal",  legend.key=element_rect(size=7),legend.key.size = unit(1.5, "lines"))
      
      #remove legend sutitle:
      d=d+ guides(fill=guide_legend(title=NULL))
      debut_annee=c(as.numeric(as.Date("2008-01-01")),
                    as.numeric(as.Date("2009-01-01")), 
                    as.numeric(as.Date("2010-01-01")), 
                    as.numeric(as.Date("2011-01-01")), 
                    as.numeric(as.Date("2012-01-01")), 
                    as.numeric(as.Date("2013-01-01")),
                    as.numeric(as.Date("2014-01-01")), 
                    as.numeric(as.Date("2015-01-01")),
                    as.numeric(as.Date("2016-01-01")) )
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
      X$deb_sem=as.Date(X$deb_sem,origin="1970-01-01")
      X[,weeks:=week(deb_sem)]
      setnames(X,"deb_sem","Date")
      d= ggplot(data=X,aes(x=Date,y=value,fill=Légende)) 
      d=d + geom_bar(stat = "identity") 
      #increase space between legend keys:
      d= d + theme(legend.position="bottom", legend.direction="horizontal",  legend.key=element_rect(size=7),legend.key.size = unit(1.5, "lines"))
      #more vibrant color:
      d= d + scale_fill_manual(values=c("#0000ff", "#840000"))
      #remove legend sutitle:
      d=d+ guides(fill=guide_legend(title=NULL))
      debut_annee=c(as.numeric(as.Date("2008-01-01")),
                    as.numeric(as.Date("2009-01-01")), 
                    as.numeric(as.Date("2010-01-01")), 
                    as.numeric(as.Date("2011-01-01")), 
                    as.numeric(as.Date("2012-01-01")), 
                    as.numeric(as.Date("2013-01-01")),
                    as.numeric(as.Date("2014-01-01")), 
                    as.numeric(as.Date("2015-01-01")),
                    as.numeric(as.Date("2016-01-01")) )
      
      d= d + geom_vline(xintercept = debut_annee,
                        linetype=4,colour="orange")
      d=d + ggtitle(label=paste(title.label.list, myname[p]))
      d=d + xlab("Date (ligne orange=1er janvier)") + ylab(title.ylab)
      myplot[[p]]=d
     print(myplot[[p]])
    }
  }
  
}