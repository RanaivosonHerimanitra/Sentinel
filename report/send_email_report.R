setwd("/media/herimanitra/Document/IPM_sentinelle/sentinel_hrmntr 291115/Sentinel/report")
#elements of the email:
destinataires=c("rherimanitra@pasteur.mg","fgirond@pasteur.mg","laurence@pasteur.mg","rrandrem@pasteur.mg","leabricette@pasteur.mg","ppiola@pasteur.mg")
require(mailR)
send.mail(from = "airmanitra@gmail.com",
          to = destinataires,
          subject = paste0("Test d'envoi automatique des Rapports sentinelle du ", Sys.Date()),
          body = "Bonjour,\n Ci-joint les rapports sentinelle de cette semaine, envoyés automatiquement à l'aide du package mailR (https://github.com/rpremraj/mailR). \n Herimanitra, ",
          smtp = list(host.name = "smtp.gmail.com", port = 465, 
                      user.name = "airmanitra@gmail.com", 
                      passwd = "****", ssl = TRUE),
          authenticate = TRUE,
          send = TRUE,
          attach.files = c("report.zip"),
          debug = TRUE)