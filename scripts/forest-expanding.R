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
all_forecasts <- seq(as.Date("1999/4/1"), as.Date("2020/1/1"), "month")
all_forecasts <- as.data.frame(all_forecasts)
names(all_forecasts) <- c("date")
horizons <- c(3, 6, 12)


tic("expanding horizon forest")
for (horizon in horizons) {
  forecasts_rf <- c()
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
    var_mbd <- embed(var_ts, (other_vars_lag_order - 1))
    var_mbd <- as.data.frame(var_mbd)
    infl_mbd <- cbind(infl_mbd, var_mbd)
  }
  
  #set initial training and test sets
  y_train <- infl_mbd[,1]
  X_train <- infl_mbd[,-1]
  X_test <- infl_mbd[nrow(infl_mbd), c(1:(lag_order + 11*length(names(other_vars)) + 1))]
  
  # here is where we reshape the training data to reflect the time distance
  # corresponding to the current forecast horizon.
  y_train <- y_train[-c(1:(horizon - 1))] 
  X_train <- X_train[-c((nrow(X_train) - (horizon - 2)):nrow(X_train)), ] 

    set.seed(1960)
    # fit the model
    fit_rf <- randomForest(X_train, y_train)
    # predict using the test set
    predict_rf <- predict(fit_rf, X_test)

  forecasts_rf <- c(forecasts_rf, predict_rf)
  
}
  forecasts_rf <- as.data.frame(forecasts_rf)
  names(forecasts_rf) <- c("prediction")
  if (horizon == 3) {
  forecasts_rf <- forecasts_rf %>% 
    dplyr::mutate(date = seq(as.Date("1999-04-01"), as.Date("2019-04-01"), "month")) %>% 
    dplyr::select(month3 = prediction, date)
  } else if (horizon == 6) {
    forecasts_rf <- forecasts_rf %>% 
      dplyr::mutate(date = seq(as.Date("1999-07-01"), as.Date("2019-07-01"), "month")) %>% 
      dplyr::select(month6 = prediction, date)
  } else if (horizon == 12) {
    forecasts_rf <- forecasts_rf %>% 
      dplyr::mutate(date = seq(as.Date("2000-01-01"), as.Date("2020-01-01"), "month")) %>% 
      dplyr::select(month12 = prediction, date)
  }
  
  all_forecasts <- dplyr::left_join(all_forecasts, forecasts_rf, by = "date")
  
}
toc()

all_forecasts <- all_forecasts %>% 
  dplyr::select(date, 
                forest_month3 = month3, 
                forest_month6 = month6, 
                forest_month12 = month12)



#export ------------------------------------------
write_rds(all_forecasts, paste0(export,"forest_expanding_horizon.rds"))
