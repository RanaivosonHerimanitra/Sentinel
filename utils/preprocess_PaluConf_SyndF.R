#############################Preprocess PaluConf SyndF ############################
cat('variables selection in Consultations...')
Consultations=Consultations[,include,with=F]
cat('DONE\n')

cat("variables selection in PaluConf...")
PaluConf=PaluConf[,include,with=F]
cat('DONE\n')

cat("variables selection in SyndF...")
SyndF=SyndF[,include,with=F]
cat('DONE\n')

cat('reshape Consultations...')
Consultations=as.data.table(gather(Consultations,
                                   key=sites,
                                   value=nb_consultation,-c(code,deb_sem)))
cat('DONE\n')
cat('reshape PaluConf...')
PaluConf=as.data.table(gather(PaluConf,
                              key=sites,
                              value=malaria_cases,-c(code,deb_sem)))
cat('DONE\n')
cat('reshape SyndF...')
SyndF=as.data.table(gather(SyndF,
                           key=sites,
                           value=nb_fievre,-c(code,deb_sem)))
cat('DONE\n')
cat("merge PaluConf and SyndF...")
PaluConf_SyndF=merge(PaluConf,
                     SyndF[,list(sites,deb_sem,nb_fievre)],
                     by.x=c("sites","deb_sem"),
                     by.y=c("sites","deb_sem"),all.x=T)
cat('DONE\n')

cat("merge PaluConf_SyndF and Consultations...")
PaluConf_SyndF=merge(PaluConf_SyndF,
                     Consultations[,list(sites,deb_sem,nb_consultation)],
                     by.x=c("sites","deb_sem"),
                     by.y=c("sites","deb_sem"),all.x=T)
cat('DONE\n')

cat('Extract weeks and years from PaluConf...')
PaluConf_SyndF[,weeks:=as.numeric(substr(code,6,8))]
PaluConf_SyndF[,years:=as.numeric(substr(code,1,4))]
cat('DONE\n')
cat("convert site to character...")
PaluConf_SyndF[,sites:=as.character(sites)]
cat("DONE\n")
save(PaluConf_SyndF,file=paste0("temp/PaluConf_SyndF",".rda"))