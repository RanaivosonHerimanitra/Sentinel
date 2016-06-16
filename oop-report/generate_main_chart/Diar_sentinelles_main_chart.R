source("reporting_plot.R",local = T) #for plotting nice graphics
p=generate_plot(htc="all",
                mydata=mydata,
                disease1.targetvar="diarh_feb",
                disease2.targetvar="diarh",
                legend.disease1="Diarrhées fébriles",
                legend.disease2="Diarrhées non fébriles",
                title.label="Diarrhées dans le réseau sentinelle",
                title.ylab="Cas de Diarrhées")
print(p)