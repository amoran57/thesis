# Header ---------------------------------------
rm(list=ls()) 
header <- source("header.R")

#Code ------------------------------------------
#simple AR model
yt <- arima.sim(list(order=c(1,0,0), ar=0.9), n=511)

#evolving AR model
x <- c(1:5)
zt <- c(0.5)
for (i in 1:510) {
  this_beta <- 0.9 + sample(x,1)*0.01
  this_number <- this_beta*zt[i] + rnorm(1)
  zt <- c(zt, this_number)
}

zt <- ts(zt)

#Export ---------------------------------------
write_rds(yt, paste0(simulate_ar1, "ar1-data/simulated-data.rds"))
write_rds(zt, paste0(simulate_ar1, "evolving-data/simulated-evolving.rds"))
