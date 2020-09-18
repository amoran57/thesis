# Header ---------------------------------------
rm(list=ls()) 
header <- source("header.R")

#Code ------------------------------------------
#import data
df <- read_rds(paste0(export, "master_data.rds"))

values_df <- df %>% 
  dplyr::filter(year >= 1959)

tsData <- ts(values_df$infl, start = c(1959,1), frequency = 12)

#rolling horizon forecast by ets and auto.arima --------------------
yearly_dates <- seq(as.Date("2000/1/1"), as.Date("2019/1/1"), "year")
start_row <- 1
i <- 673   
pred_arima <- c()

for (yearx in yearly_dates) {
  #initialize training data according to expanding horizon
  train_df <- values_df %>% 
    filter(date < yearx)
  train_tsData <- ts(train_df$infl, start = c(1959, 1), frequency = 12)
  
  pred_a <- forecast(auto.arima(train_tsData), 12)$mean[12]
  
  pred_arima <- c(pred_arima, pred_a)
}

toc()

pred_arima <- as.data.frame(pred_arima[37:48])

names(pred_arima) <- "arima"


pred_arima_df <- pred_arima %>% 
  mutate(date = seq(as.Date("2019/1/1"), as.Date("2019/12/1"), "month"))

pred_arima <- ts(pred_arima$arima, start=c(2019, 1), frequency =12)

accuracy(pred_arima, tsData)

forecast_df <- left_join(values_df, pred_arima_df, by = "date")
