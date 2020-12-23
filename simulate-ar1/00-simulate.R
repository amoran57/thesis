# Header ---------------------------------------
rm(list=ls()) 
header <- source("header.R")

#Code ------------------------------------------
#simple AR model
yt <- arima.sim(list(order=c(1,0,0), ar=0.9), n=511)

zt <- arima.sim(list(order=c(1,0,0), ar=0.45), n=11)
#evolving AR model
for (i in 1:5) {
  this_beta <- 0.45 + i*0.1
  this_series <- arima.sim(list(order=c(1,0,0), ar=this_beta), n=100)
  zt <- c(zt, this_series)
}

zt <- ts(zt)

#Export ---------------------------------------
write_rds(yt, paste0(simulate_ar1, "ar1-data/simulated-data.rds"))
write_rds(zt, paste0(simulate_ar1, "evolving-data/simulated-evolving.rds"))
