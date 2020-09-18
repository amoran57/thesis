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
monthly_dates <- seq(as.Date("1999/1/1"), as.Date("2019/1/1"), "month")
lag_order <- 48
forecasts_rf <- c()

tic("expanding horizon forest")

for (monthx in monthly_dates) {
  #initialize training data according to expanding horizon
  train_df <- values_df %>% 
    filter(date <= monthx)
  train_tsData <- ts(train_df$infl, start = c(1959, 1), frequency = 12)

  infl_mbd <- embed(train_tsData, lag_order + 1)
  infl_mbd <- as.data.frame(infl_mbd)
  
  #add other variables
  other_vars_lag_order <- 12
  other_vars <- df %>% 
    filter(date >= as.Date("1962-01-01") & date < monthx) %>% 
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
  
  # here is where we reshape the training data to reflect the time distance
  # corresponding to the current forecast horizon.
  y_train <- y_train[-c(1:11)] 
  X_train <- X_train[-c((nrow(X_train) - 11):nrow(X_train)), ] 

    set.seed(1960)
    # fit the model
    fit_rf <- randomForest(X_train, y_train)
    # predict using the test set
    predict_rf <- predict(fit_rf, X_test)

  forecasts_rf <- c(forecasts_rf, predict_rf)
  
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
naive_forecast <- window(tsData, start = c(1999, 1), end = c(2018, 12))
naive_df <- as.data.frame(naive_forecast) %>% 
  select(naive = x) %>% 
  mutate(date = seq(as.Date("2000/1/1"), as.Date("2019/12/1"), "month"))

naive_ts <- ts(naive_df$naive, start = c(2000, 1), frequency = 12)
accuracy(naive_ts, tsData)
forecast_df <- left_join(forecast_df, naive_df, by = "date")

# plot results -----------------------------------
tidy_forecast <- gather(data = forecast_df, key = "key", value = "value", "infl", "forest", "naive") %>%
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
write_rds(forest_pred_df, paste0(export,"forest_expanding_horizon.rds"))
