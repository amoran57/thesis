# Header ---------------------------------------
rm(list=ls()) 
header <- source("header.R")

#Code ------------------------------------------
#import
df <- read.csv(paste0(import, "CPIAUCSL.csv")) %>% 
  select(date = DATE, cpi = CPIAUCSL) %>% 
  mutate(date = as.Date(date),
         l_cpi = log(cpi),
         l_cpi_1 = dplyr::lag(l_cpi),
         infl = l_cpi - l_cpi_1) %>% 
  select(date, infl)

#set as time series object
tsData <- ts(df$infl[-1], start = c(1947,2), frequency = 12)
tsData <- window(tsData, start = c(1959,1))
#test residuals
adf.test(tsData)
dickey_fuller <- ur.df(tsData, type = "none", lags = 0)
summary(dickey_fuller)
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
auto.arima(tsData, d = 0, max.P = 0, max.Q = 0, ic = "aic", stepwise = FALSE, approximation = FALSE)
auto.arima(tsstationary, d = 0, max.P = 0, max.Q = 0, stepwise = FALSE, approximation = FALSE)

model <- arma(df$infl[-1], order = c(1,2))
summary(model)

predictions <- df[-1,]
predictions$arma12 <- model$fitted.values

#Check for structural break ------------------------------
tsData <- window(tsData, start = c(1989, 1))
library(strucchange)
test2 <- Fstats(tsData~stats::lag(tsData)) #Gets a sequence of fstatistics for all possible break points within the middle 70% of tsData
tsData.fs <- test2$Fstats #These are the fstats
bp.tsData <- breakpoints(tsData~stats::lag(tsData), breaks = length(test2$breakpoint)) #Gets the breakpoint based on the F-stats
plot(tsData) #plots the series tsData
lines(bp.tsData) #plots the break date implied by the sup F test
bd.tsData <- breakdates(bp.tsData) #Obtains the implied break data
sctest(test2) #Obtains a p-value for the implied breakpoint
ci.tsData <- confint(bp.tsData) #95% CI for the location break date
plot(tsData)
lines(ci.tsData) #This shows the interval around the estimated break date

years <- seq(1989,2010)
breakpoints <- list()
for(i in 1:length(years)) {
  year <- years[i]
  this_tsData <- window(tsData, start = c(year, 1), end = c(year+10, 1))
  test2 <- Fstats(this_tsData~stats::lag(this_tsData)) #Gets a sequence of fstatistics for all possible break points within the middle 70% of tsData
  this_tsData.fs <- test2$Fstats #These are the fstats
  bp.this_tsData <- breakpoints(this_tsData~stats::lag(this_tsData), breaks = length(test2$breakpoint)) #Gets the breakpoint based on the F-stats
  breakpoints[i] <- bp.this_tsData$breakpoints
}

these_years <- years[which(!is.na(breakpoints))]
for(i in 1:length(these_years)) {
  year <- these_years[i]
  this_tsData <- window(tsData, start = c(year, 1), end = c(year+9, 1))
  test2 <- Fstats(this_tsData~stats::lag(this_tsData)) #Gets a sequence of fstatistics for all possible break points within the middle 70% of tsData
  this_tsData.fs <- test2$Fstats #These are the fstats
  bp.this_tsData <- breakpoints(this_tsData~stats::lag(this_tsData)) #Gets the breakpoint based on the F-stats
  plot(this_tsData) #plots the series this_tsData
  lines(bp.this_tsData) #plots the break date implied by the sup F test
  bd.this_tsData <- breakdates(bp.this_tsData) #Obtains the implied break data
  sctest(test2) #Obtains a p-value for the implied breakpoint
  ci.this_tsData <- confint(bp.this_tsData) #95% CI for the location break date
  plot(this_tsData)
  lines(ci.this_tsData) #This shows the interval around the estimated break date
}

#Check for time trend -------------------------------------
pastecs::trend.test(tsData)
x <- seq_along(tsData)
time_model <- lm(tsData ~ x)
summary(time_model)
plot(tsData)
lines(x/12 + 1990, time_model$fitted.values, col = "red")
