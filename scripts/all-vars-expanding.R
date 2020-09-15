# Header ---------------------------------------
rm(list=ls()) 
header <- source("header.R")

#Code ------------------------------------------
#import data
df <- read_rds(paste0(export, "master_data.rds"))

values_df <- df %>% 
  dplyr::filter(year >= 1959)

tsData <- ts(values_df$infl, start = c(1959,1), frequency = 12)

#random forest method --------------------------------------
yearly_dates <- seq(as.Date("2000/1/1"), as.Date("2019/1/1"), "year")
test_vector <- c()
lag_order <- 48
horizon <- 12

forecasts_rf <- c()

tic("expanding horizon forest")
for (yearx in yearly_dates) {
  #initialize training data according to expanding horizon
  train_df <- values_df %>% 
    filter(date < yearx)
  train_tsData <- ts(train_df$infl, start = c(1959, 1), frequency = 12)

  infl_mbd <- embed(train_tsData, lag_order + 1)
  infl_mbd <- as.data.frame(infl_mbd)
  
  #add other variables
  other_vars_lag_order <- 12
  other_vars <- df %>% 
    filter(date >= as.Date("1962-01-01") & date < yearx) %>% 
    select(-c(date, infl, infl_na, rate12month, spread, survey, year))
  other_vars <- other_vars[-nrow(other_vars),]
  
  #add other variables to embedded matrix
  for (var in names(other_vars)) {
    var_ts <- ts(other_vars[var], start = c(1962, 1), frequency = 12)
    var_mbd <- embed(var_ts, other_vars_lag_order)
    var_mbd <- as.data.frame(var_mbd)
    infl_mbd <- cbind(infl_mbd, var_mbd)
  }
  
  #set initial training and test sets
  y_train <- infl_mbd[,1]
  X_train <- infl_mbd[,-1]
  X_test <- infl_mbd[nrow(infl_mbd), c(1:(lag_order + 12*length(names(other_vars)) + 1))]
  
  temp_forecasts_rf <- numeric(horizon)
  
  for (i in 1:horizon){
    set.seed(1960)
    # fit the model
    fit_rf <- randomForest(X_train, y_train)
    # predict using the test set
    temp_forecasts_rf[i] <- predict(fit_rf, X_test)
    # here is where we repeatedly reshape the training data to reflect the time distance
    # corresponding to the current forecast horizon.
    y_train <- y_train[-1] 
    X_train <- X_train[-nrow(X_train), ] 
  }
  
  forecasts_rf <- c(forecasts_rf, temp_forecasts_rf)
  }
toc()


y_pred <- ts(forecasts_rf, start = c(2000, 1), frequency = 12)

accuracy(y_pred, tsData)

forest_pred_df <- as.data.frame(y_pred) %>% 
  select(forest = x) %>% 
  mutate(date = seq(as.Date("2000/1/1"), as.Date("2019/12/1"), "month")
  )

forecast_df <- left_join(values_df, forest_pred_df, by = "date")
# naive model ------------------------------------
# naive_df <- values_df %>% 
#   filter(year == 2018) %>% 
#   select(date, infl) %>% 
#   mutate(new_date = date + 365) %>% 
#   select(date = new_date, naive = infl)
# 
# naive_ts <- ts(naive_df$naive, start = c(2019, 1), frequency = 12)
# forecast_df <- left_join(forecast_df, naive_df, by = "date")

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

tidy_forecast <- gather(data = forecast_df, key = "key", value = "value", "infl", "arima":"naive") %>% 
  filter(year > 2016 & year < 2020)

plot_all <- ggplot(data = tidy_forecast, aes(x = date, y = value, color = key)) +
  geom_line() +
  scale_color_manual(values = c("green", "red", "blue", "black", "gray")) +
  theme_minimal() +
  labs(
    title = "Forecasted monthly inflation",
    subtitle = "Predicted for 2019 given 1959-2018 data",
    x = "Date",
    y = "Inflation"
  )

plot_fc
plot_all

#we find the ARIMA method has the lowest 
#RMSE, 45% lower than the naive model
rmse <- c(accuracy(pred_ets, tsData)[2],
          accuracy(pred_arima, tsData)[2],
          accuracy(y_pred, tsData)[2],
          accuracy(naive_ts, tsData)[2])

names(rmse) <- c("ets", "arima", "forest", "naive")
rmse["forest"]/rmse["naive"]
rmse["arima"]/rmse["naive"]
rmse["ets"]/rmse["naive"]


#export ------------------------------------------
write_rds(forest_pred_df, paste0(export,"forest_expanding_horizon.rds"))
