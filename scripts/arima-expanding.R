# Header ---------------------------------------
rm(list=ls()) 
header <- source("header.R")

#Code ------------------------------------------
#import data
df <- read_rds(paste0(export, "master_data.rds"))

values_df <- df %>% 
  dplyr::filter(year >= 1959)

tsData <- ts(values_df$infl, start = c(1959,1), frequency = 12)

#rolling horizon forecast by auto.arima --------------------
monthly_dates <- seq(as.Date("1999/1/1"), as.Date("2019/1/1"), "month")
pred_arima <- c()
all_forecasts <- seq(as.Date("1999/4/1"), as.Date("2020/1/1"), "month")
all_forecasts <- as.data.frame(all_forecasts)
names(all_forecasts) <- c("date")
horizons <- c(3, 6, 12)

tic("arima")
for (horizon in horizons) {
  pred_arima <- c()
for (monthx in monthly_dates) {
  #initialize training data according to expanding horizon
  train_df <- values_df %>% 
    filter(date <= monthx)
  train_tsData <- ts(train_df$infl, start = c(1959, 1), frequency = 12)

  pred_a <- forecast(auto.arima(train_tsData), 12)$mean[12]
  
  pred_arima <- c(pred_arima, pred_a)
}
  
  pred_arima <- as.data.frame(pred_arima)
  names(pred_arima) <- c("prediction")
  if (horizon == 3) {
    pred_arima <- pred_arima %>% 
      dplyr::mutate(date = seq(as.Date("1999-04-01"), as.Date("2019-04-01"), "month")) %>% 
      dplyr::select(month3 = prediction, date)
  } else if (horizon == 6) {
    pred_arima <- pred_arima %>% 
      dplyr::mutate(date = seq(as.Date("1999-07-01"), as.Date("2019-07-01"), "month")) %>% 
      dplyr::select(month6 = prediction, date)
  } else if (horizon == 12) {
    pred_arima <- pred_arima %>% 
      dplyr::mutate(date = seq(as.Date("2000-01-01"), as.Date("2020-01-01"), "month")) %>% 
      dplyr::select(month12 = prediction, date)
  }
  
  all_forecasts <- dplyr::left_join(all_forecasts, pred_arima, by = "date")
  
}
toc()


#export ------------------------------------------
write_rds(pred_df, paste0(all_forecasts,"arima_expanding_horizon.rds"))

