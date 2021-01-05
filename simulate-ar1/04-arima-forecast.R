# Header ---------------------------------------
rm(list=ls()) 
header <- source("header.R")

#Set up for tree -------------------------------
yt <- read_rds(paste0(simulate_ar1, "ar1-data/simulated-data.rds"))
zt <- read_rds(paste0(simulate_ar1, "evolving-data/simulated-evolving.rds"))
yt <- ts(yt)
zt <- ts(zt)
#Predict using ARIMA --------------------------------------
dates <- seq(410,510)
forecasts_y <- c()
forecasts_z <- c()
tic("expanding horizon y")
for (datex in dates) {
  #initialize training data according to expanding horizon
  train <- yt[1:datex]
 
  #make the model
  model <- auto.arima(train)
  
  #get the prediction
  prediction <- predict(model, 1)
  
  forecasts_y <- c(forecasts_y, prediction)
}
toc()

tic("expanding horizon z")
for (datex in dates) {
  #initialize training data according to expanding horizon
  train <- zt[1:datex]
  
  #make the model
  model <- auto.arima(train)
  
  #get the prediction
  prediction <- predict(model, 1)
  
  forecasts_z <- c(forecasts_z, prediction)
}
toc()


#Export ---------------------------------------
write_rds(forecasts_y, paste0(simulate_ar1, "ar1-data/arima-forecast.rds"))
write_rds(forecasts_z, paste0(simulate_ar1, "evolving-data/arima-evolving.rds"))