# Header ---------------------------------------
rm(list=ls()) 
header <- source("header.R")

#Code ------------------------------------------
#import
df <- read.csv(paste0(thesis, "CPIAUCSL.csv")) %>% 
  select(date = DATE, cpi = CPIAUCSL) %>% 
  mutate(date = as.Date(date),
         l_cpi = log(cpi),
         l_cpi_1 = dplyr::lag(l_cpi),
         infl = l_cpi - l_cpi_1) %>% 
  select(date, infl)

#set as time series object
tsData <- ts(df$infl[-1], start = c(1947,2), frequency = 12)

#test residuals
adf.test(tsData)
urkpssTest(tsData, type = c("tau"), lags = c("short"),use.lag = NULL, doplot = TRUE)

#plot first difference
tsstationary <- diff(tsData, differences=1)
plot(tsstationary)

#observe ACF and PACF for series and differenced series
acf(tsData,lag.max=34)
pacf(tsData,lag.max=34)
acf(tsstationary, lag.max = 34)
pacf(tsstationary, lag.max = 34)

#get the best model for each
auto.arima(tsData, d = 0, max.P = 0, max.Q = 0, stepwise = FALSE, approximation = FALSE)
auto.arima(tsstationary, d = 0, max.P = 0, max.Q = 0, stepwise = FALSE, approximation = FALSE)

model <- arma(df$infl[-1], order = c(1,2))
summary(model)

predictions <- df[-1,]
predictions$arma12 <- model$fitted.values
