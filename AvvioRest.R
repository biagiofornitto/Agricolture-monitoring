#serve ad avviare i servizi rest

library(plumber)
r <- plumb(paste(getwd(),"progetto_finale\\SubscriberREST.R",sep="\\"))
r$run(port=8000)