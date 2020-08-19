# Header ---------------------------------------
rm(list=ls()) 
header <- source("header.R")

#Code ------------------------------------------
df <- read.csv(paste0(thesis, "CPIAUCSL.csv")) %>% 
  select(date = DATE, cpi = CPIAUCSL) %>% 
  mutate(date = as.Date(date),
         l_cpi = log(cpi),
         l_cpi_1 = lag(l_cpi),
         infl = l_cpi - l_cpi_1) %>% 
  select(date, infl)

tsData <- ts(df$infl[-1], start = c(1947,2), frequency = 12)
components_ts <- decompose(tsData)
plot(components_ts)

urkpssTest(tsData, type = c("tau"), lags = c("short"),use.lag = NULL, doplot = TRUE)
tsstationary <- diff(tsData, differences=1)
plot(tsstationary)

acf(tsData,lag.max=34)
pacf(tsData,lag.max=34)

auto.arima(tsData)

model <- arma(df$infl[-1], order = c(2,4))
summary(model)

predictions <- df[-1,]
predictions$arma2 <- model$fitted.values
