# Header ---------------------------------------
rm(list=ls()) 
header <- source("header.R")

#Code ------------------------------------------
df <- read_rds(paste0(export, "master_data.rds")) 

values_df <- df %>% 
  filter(year >= 1962) %>% 
  dplyr::select(date, infl, rate10yr, unemp, nat_unemp, year)


tsData <- ts(values_df$infl, start = c(1962,1), frequency = 12)

#Set model ----------------------------------
monthly_dates <- seq(as.Date("1999/1/1"), as.Date("2019/1/1"), "month")
var_pred <- c()

tic("var")
for (monthx in monthly_dates) {
  df_train <- values_df %>% 
    filter(date <= monthx) %>% 
    dplyr::select(-date, -year)
  
  df_train_ts <- ts(df_train, start = c(1962, 1), frequency = 12)
  
  model <- VAR(df_train_ts, lag.max = 6, ic = "AIC", type = "both")
  
  
  prediction <- predict(model, n.ahead = 12)
  prediction <- prediction$fcst
  prediction <- as.data.frame(prediction) %>% 
    dplyr::select(infl = infl.fcst)
  value <- prediction[12,]
  
  var_pred <- c(var_pred, value)
}
toc()


y_pred <- ts(var_pred, start = c(2000, 1), frequency = 12)

accuracy(y_pred, tsData)


pred_df <- as.data.frame(y_pred) %>% 
  dplyr::select(var = x) %>% 
  mutate(date = seq(as.Date("2000/1/1"), as.Date("2020/1/1"), "month")
  )

forecast_df <- left_join(values_df, pred_df, by = "date")
# naive model ------------------------------------
naive_forecast <- window(tsData, start = c(1999, 1), end = c(2018, 12))
naive_df <- as.data.frame(naive_forecast) %>% 
  dplyr::select(naive = x) %>% 
  mutate(date = seq(as.Date("2000/1/1"), as.Date("2019/12/1"), "month"))

naive_ts <- ts(naive_df$naive, start = c(2000, 1), frequency = 12)
accuracy(naive_ts, tsData)
forecast_df <- left_join(forecast_df, naive_df, by = "date")

# plot results -----------------------------------
tidy_forecast <- gather(data = forecast_df, key = "key", value = "value", "infl", "var", "naive") %>%
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
write_rds(pred_df, paste0(export,"var_expanding_horizon.rds"))


