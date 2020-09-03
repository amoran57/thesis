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

#rolling horizon forecast by ets and auto.arima --------------------
start_row <- 673
i <- 12    #number of months of training data, to start 
pred_ets <- c()
pred_arima <- c()
while(i <= 48){
  ts <- ts(values_df[start_row:(start_row + i), "infl"], start=c(2015, 1), frequency=12)
  
  pred_e <- forecast(ets(ts), 12)$mean[c(10:12)]
  pred_a <- forecast(auto.arima(ts), 12)$mean[c(10:12)]
  
  pred_ets <- c(pred_ets, pred_e)
  pred_arima <- c(pred_arima, pred_a)
  
  i = i + 3
}

pred_ets <- as.data.frame(pred_ets)
pred_arima <- as.data.frame(pred_arima)

names(pred_arima) <- "arima"
names(pred_ets) <- "ets"

pred_arima_df <- pred_arima %>% 
  mutate(date = seq(as.Date("2016/10/1"), as.Date("2019/12/1"), "month")
)
pred_ets_df <- pred_ets %>% 
  mutate(date = seq(as.Date("2016/10/1"), as.Date("2019/12/1"), "month")
)

pred_ets <- ts(pred_ets$ets, start=c(2016, 10), frequency = 12)
pred_arima <- ts(pred_arima$arima, start=c(2016, 10), frequency =12)

accuracy(pred_ets, tsData)
accuracy(pred_arima, tsData)

forecast_df <- left_join(values_df, pred_arima_df, by = "date")
forecast_df <- left_join(forecast_df, pred_ets_df, by = "date")

#random forest method --------------------------------------
#note that this section of code is shamelessly plagiarized
#from https://www.r-bloggers.com/time-series-forecasting-with-random-forest/

train_df <- values_df %>% 
  filter(year < 2019)
train_tsData <- ts(values_df$infl, start = c(1959, 7), frequency = 12)

lag_order <- 48
horizon <- 12

infl_mbd <- embed(train_tsData, lag_order + 1)

y_train <- infl_mbd[,1]
X_train <- infl_mbd[,-1]
y_test <- window(tsData, start = c(2019, 1), end = c(2019, 12))
X_test <- infl_mbd[nrow(infl_mbd), c(1:lag_order)]

forecasts_rf <- numeric(horizon)

for (i in 1:horizon){
  set.seed(1960)
  # fit the model
  fit_rf <- randomForest(X_train, y_train)
  # predict using the test set
  forecasts_rf[i] <- predict(fit_rf, X_test)
  # here is where we repeatedly reshape the training data to reflect the time distance
  # corresponding to the current forecast horizon.
  y_train <- y_train[-1] 
  X_train <- X_train[-nrow(X_train), ] 
}

y_pred <- ts(forecasts_rf, start = c(2019, 1), frequency = 12)

forest_pred_df <- as.data.frame(y_pred) %>% 
  select(forest = x) %>% 
  mutate(date = seq(as.Date("2019/1/1"), as.Date("2019/12/1"), "month")
  )

forecast_df <- left_join(forecast_df, forest_pred_df, by = "date")

# naive model ------------------------------------
naive_df <- values_df %>% 
  filter(year == 2018) %>% 
  select(date, infl) %>% 
  mutate(new_date = date + 365) %>% 
  select(date = new_date, naive = infl)

naive_ts <- ts(naive_df$naive, start = c(2019, 1), frequency = 12)
forecast_df <- left_join(forecast_df, naive_df, by = "date")

# plot results -----------------------------------
plot_fc <- forecast_df %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = infl)) +
  geom_line(aes(y = forest), color = "blue") +
  theme_minimal() +
  labs(
    title = "Forecasted inflation for 2019",
    x = "Date",
    y = "Inflation"
  )

tidy_forecast <- gather(data = forecast_df, key = "key", value = "value", "infl", "forest":"naive") %>% 
  filter(year > 2009 & year < 2020)

plot_all <- ggplot(data = tidy_forecast, aes(x = date, y = value, color = key)) +
  geom_line() +
  scale_color_manual(values = c("green", "red", "blue", "black", "gray")) +
  theme_minimal() +
  labs(
    title = "Forecasted monthly inflation",
    subtitle = "Predicted for 2019 given 2015-2018 data",
    x = "Date",
    y = "Inflation"
  )

plot_fc
plot_all

#we find the random forest method has the lowest 
#RMSE, 35% lower than the naive model
rmse <- c(accuracy(pred_ets, tsData)[2],
accuracy(pred_arima, tsData)[2],
accuracy(y_pred, tsData)[2],
accuracy(naive_ts, tsData)[2])

names(rmse) <- c("ets", "arima", "forest", "naive")
rmse["forest"]/rmse["naive"]

#export ------------------------------------------
write_rds(forecast_df, paste0(thesis,"forest_naive_2019_4year_forecast.rds"))
