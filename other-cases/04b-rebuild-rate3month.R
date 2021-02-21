# Header ---------------------------------------
rm(list=ls())
header <- source("header.R")

#Code ------------------------------------------
us_df <- read_rds(paste0(export, "master_data.rds"))
us_values <- us_df %>% 
  select(date, unemp, rate3month)
#Test US 3-month rate values for stationarity -------------------
rate_ts <- ts(us_values$rate3month, start = c(1947, 1), frequency = 12)

#It is not stationary. Let's try to detrend it:
rate_df <- us_values %>% select(date, rate = rate3month)
rownames(rate_df) <- seq(1, nrow(rate_df))
rate_df$trend <- seq(1, nrow(rate_df))

#Again the trend is more important
rate_trend_model <- glm(rate ~ trend, data = rate_df)
summary(rate_trend_model)

#To retrend, all I have to do is add the residuals to the fitted values
fitted <- rate_trend_model$fitted.values
residuals <- rate_trend_model$residuals
real_values <- fitted + residuals

detrend_rate <- rate_trend_model$residuals
detrend_rate_ts <- ts(detrend_rate, start = c(1947,1), frequency = 12)


#Next I took the difference of the detrended residuals
diff_rate_ts <- diff(detrend_rate_ts)
plot(diff_rate_ts)

#I just have to undo this to get the original values back
original_ts <- data.frame(date = seq(as.Date("1947/1/1"), as.Date("2020/8/1"), "month"), core = detrend_rate_ts, diff = c(NA_real_, diff_rate_ts), naive_diff = lag(c(NA_real_, diff_rate_ts)))
original_ts$lag_original <- original_ts$core - original_ts$diff
original_ts$lag_naive <- original_ts$core - original_ts$naive_diff

# Import and undifference -----------------------------------------
#So all I have to do is bring in the predicted values from this, and run them backwards through the system
arima_forecast <- read_rds(paste0(export, "other_cases/rate3month/arima_forecast.rds"))
forest_forecast <- read_rds(paste0(export, "other_cases/rate3month/forecast.rds"))
ar1_forecast <- read_rds(paste0(export, "other_cases/rate3month/ar1_forecast.rds"))
base_forecast <- read_rds(paste0(export, "other_cases/rate3month/base_forecast.rds"))

forecast_df <- data.frame(date = seq(as.Date("1990/1/1"), as.Date("2000/1/1"), "month"), 
                          arima = arima_forecast, 
                          forest = forest_forecast,
                          ar1 = ar1_forecast,
                          base = base_forecast)

#Find the lags of the differenced forecasts
full_df <- left_join(original_ts, forecast_df, by = "date")
full_df$lag_arima_forecast <- full_df$core - full_df$arima
full_df$lag_forest_forecast <- full_df$core - full_df$forest
full_df$lag_ar1_forecast <- full_df$core - full_df$ar1
full_df$lag_base_forecast <- full_df$core - full_df$base

#Delag them
detrend_arima_forecast <- ts(full_df$lag_ar1_forecast[-c(1:456)], start = c(1984, 12), frequency = 12)
detrend_forest_forecast <- ts(full_df$lag_ar1_forecast[-c(1:456)], start = c(1984, 12), frequency = 12)
detrend_ar1_forecast <- ts(full_df$lag_ar1_forecast[-c(1:456)], start = c(1984, 12), frequency = 12)
detrend_base_forecast <- ts(full_df$lag_ar1_forecast[-c(1:456)], start = c(1984, 12), frequency = 12)
detrend_naive_forecast <- ts(original_ts$lag_naive[-c(1:2)], start = c(1947, 2), frequency = 12)

#Detrend them to get the original forecast
fitted <- rate_trend_model$fitted.values
fitted_ts <- ts(fitted, start = c(1947, 1), frequency = 12)
this_fitted_ts <- window(fitted_ts, start = c(1984, 12), end = c(1994, 12))

arima_forecast <- this_fitted_ts + detrend_arima_forecast
forest_forecast <- this_fitted_ts + detrend_forest_forecast
ar1_forecast <- this_fitted_ts + detrend_ar1_forecast
base_forecast <- this_fitted_ts + detrend_base_forecast
naive_forecast <- this_fitted_ts + detrend_naive_forecast

# Compare values ---------------------------
accuracy(rate_ts, arima_forecast)
accuracy(rate_ts, forest_forecast)
accuracy(rate_ts, ar1_forecast)
accuracy(rate_ts, base_forecast)
accuracy(rate_ts, naive_forecast)

# Export ------------------------------
write_rds(arima_forecast, paste0(export, "other_cases/rate3month/retransformed_forecasts/arima_forecast.rds"))
write_rds(forest_forecast, paste0(export, "other_cases/rate3month/retransformed_forecasts/forest_forecast.rds"))
write_rds(ar1_forecast, paste0(export, "other_cases/rate3month/retransformed_forecasts/ar1_forecast.rds"))
write_rds(base_forecast, paste0(export, "other_cases/rate3month/retransformed_forecasts/base_forecast.rds"))
write_rds(naive_forecast, paste0(export, "other_cases/rate3month/retransformed_forecasts/naive_forecast.rds"))
