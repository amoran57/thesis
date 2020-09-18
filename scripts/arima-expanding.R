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

for (monthx in monthly_dates) {
  #initialize training data according to expanding horizon
  train_df <- values_df %>% 
    filter(date <= monthx)
  train_tsData <- ts(train_df$infl, start = c(1959, 1), frequency = 12)

  pred_a <- forecast(auto.arima(train_tsData), 12)$mean[12]
  
  pred_arima <- c(pred_arima, pred_a)
}

toc()

y_pred <- ts(pred_arima, start = c(2000, 1), frequency = 12)

accuracy(y_pred, tsData)

pred_df <- as.data.frame(y_pred) %>% 
  select(arima = x) %>% 
  mutate(date = seq(as.Date("2000/1/1"), as.Date("2019/12/1"), "month")
  )

forecast_df <- left_join(values_df, pred_df, by = "date")

# naive model ------------------------------------
naive_forecast <- window(tsData, start = c(1999, 1), end = c(2018, 12))
naive_df <- as.data.frame(naive_forecast) %>% 
  select(naive = x) %>% 
  mutate(date = seq(as.Date("2000/1/1"), as.Date("2019/12/1"), "month"))

naive_ts <- ts(naive_df$naive, start = c(2000, 1), frequency = 12)
accuracy(naive_ts, tsData)
forecast_df <- left_join(forecast_df, naive_df, by = "date")


# plot results -----------------------------------
tidy_forecast <- gather(data = forecast_df, key = "key", value = "value", "infl", "arima", "naive") %>%
  filter(year > 1999 & year < 2020)

plot_all <- ggplot(data = tidy_forecast, aes(x = date, y = value, color = key)) +
  geom_line() +
  scale_color_manual(values = c("blue", "black", "red")) +
  theme_minimal() +
  labs(
    title = "Forecasted monthly inflation",
    subtitle = "Predicted for 2000-2019 given 1959-2018 data",
    x = "Date",
    y = "Inflation"
  )

plot_all

#export ------------------------------------------
write_rds(pred_df, paste0(export,"arima_expanding_horizon.rds"))

