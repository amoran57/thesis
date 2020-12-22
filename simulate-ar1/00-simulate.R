# Header ---------------------------------------
rm(list=ls()) 
header <- source("header.R")

#Code ------------------------------------------
yt <- arima.sim(list(order=c(1,0,0), ar=0.9), n=511)

#Export ---------------------------------------
write_rds(yt, paste0(simulate_ar1, "simulated-data.rds"))
