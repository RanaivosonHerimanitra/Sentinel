#plotting...
require(ggplot2)
tmp1 = Malaria[,list(code,deb_sem,occurence)];
setnames(tmp1,"occurence","cases")
tmp1[,Disease:="Palu(TDR+)"]
setorder(tmp1,deb_sem)
tmp2 = ili[,list(code,deb_sem,Synd_g)];setnames(tmp2,"Synd_g","cases")
tmp2[,Disease:="Fièvre"]
setorder(tmp2,deb_sem)
tmp=rbind(tmp1,tmp2)
tmp=tmp[deb_sem>=as.Date("2009-01-01")]
p =ggplot(data = tmp,aes(x=as.Date(deb_sem,origin="1970-01-01"),
                         y=cases,fill=Disease))
#increase space between legend keys:
p= p + theme(legend.position="bottom", legend.direction="horizontal",  legend.key=element_rect(size=7),legend.key.size = unit(1.5, "lines"))
#remove legend title:
p=p+ guides(fill=guide_legend(title=NULL))
p= p  + geom_bar(stat = "identity")
p = p + scale_fill_manual(values=c("blue","darkred"))
p=p  + xlab("Date(Semaine)") + ylab("Nombre de cas")
#add custom vertical line for each beginning of the year:
for ( u in paste0(2009:2016,"-01-01") )
{
  p= p + geom_vline(xintercept = as.numeric(as.Date(u,origin = "1970-01-01")),size=0.75,linetype=4,colour="orange")
}
p = p + scale_x_date(breaks = date_breaks("1 year"),date_labels = "%Y")
save(p,file="report/palu_chart.rda")
p