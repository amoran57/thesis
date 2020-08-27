# Header ---------------------------------------
rm(list=ls()) 
header <- source("header.R")

#Code ------------------------------------------
#import data
df <- read.csv(paste0(thesis, "CPIAUCSL.csv")) %>% 
  select(date = DATE, cpi = CPIAUCSL) %>% 
  mutate(date = as.Date(date),
         l_cpi = log(cpi),
         l_cpi_1 = dplyr::lag(l_cpi),
         infl = l_cpi - l_cpi_1) %>% 
  select(date, infl)

month_3 <- read.csv(paste0(thesis, "TB3MS.csv")) %>% 
  select(DATE, rate_3 = TB3MS) %>% 
  mutate(date = as.Date(DATE)) %>% 
  select(date, rate_3)

month_12 <- read.csv(paste0(thesis, "TB1YR.csv")) %>% 
  select(DATE, rate_12 = TB1YR) %>% 
  mutate(date = as.Date(DATE)) %>% 
  select(date, rate_12)

df <- left_join(df, month_3, by = "date")
df <- left_join(df, month_12, by = "date") 

values_df <- df %>% 
  mutate(year = lubridate::year(date)) %>% 
  dplyr::filter(year >= 1959)

values_df <- values_df[-c(1:6),]

tsData <- ts(values_df$infl[-1], start = c(1959,7), frequency = 12)
tsData <- ts(tsData[1:100], start = c(1959, 7), frequency = 12)


groups <- createTimeSlices(tsData, 80, horizon = 5)
folds <- groupKFold(groups, k = 16)

control <- trainControl(method = "timeslice", index = groups$train)
model <- train(infl ~ rate_3, data = df[-1,], trControl = control)

#code pulled from stack exchange --------------------
i <- 36    #### Starting with 3 years of monthly training data 
pred_ets <- c()
pred_arima <- c()
while(i <= 100){
  ts <- ts(values_df[1:i, "infl"], start=c(1959, 7), frequency=12)
  
  pred_ets <- rbind(pred_ets, data.frame(forecast(ets(ts), 12)$mean[1:12]))
  pred_arima <- rbind(pred_arima, data.frame(forecast(auto.arima(ts), 12)$mean[1:12]))
  
  i = i + 12
}

names(pred_arima) <- "arima"
names(pred_ets) <- "ets"

pred_ets <- ts(pred_ets$ets, start=c(1962, 8), frequency = 12)
pred_arima <- ts(pred_arima$arima, start=c(1962, 8), frequency =12)

accuracy(pred_ets, tsData)
accuracy(pred_arima, tsData)
