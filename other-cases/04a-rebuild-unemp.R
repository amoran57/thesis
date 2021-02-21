# Header ---------------------------------------
rm(list=ls())
header <- source("header.R")

#Code ------------------------------------------
us_df <- read_rds(paste0(export, "master_data.rds"))

#Review the transformation process ----------------
us_values <- us_df %>% 
  select(date, unemp, rate3month)

unemp_ts <- ts(us_values$unemp[-c(1:12)], start = c(1948, 1), frequency = 12)
this_unemp_ts <- window(unemp_ts, start = c(1989, 12), end = c(1999, 12))
#Previously, I had detrended the time series
unemp_df <- us_values[-c(1:12),] %>% select(date, unemp)
rownames(unemp_df) <- seq(1, nrow(unemp_df))
unemp_df$trend <- seq(1, nrow(unemp_df))
trend_model <- glm(unemp ~ trend, data = unemp_df)
summary(trend_model)

#To retrend, all I have to do is add the residuals to the fitted values
fitted <- trend_model$fitted.values
residuals <- trend_model$residuals
real_values <- fitted + residuals

detrend_unemp <- trend_model$residuals
detrend_unemp_ts <- ts(detrend_unemp, start = c(1948, 1), frequency = 12)

#Next I took the difference of the detrended residuals
diff_unemp_ts <- diff(detrend_unemp_ts)
plot(diff_unemp_ts)

#I just have to undo this to get the original values back
original_ts <- data.frame(date = seq(as.Date("1948/1/1"), as.Date("2020/8/1"), "month"), core = detrend_unemp_ts, diff = c(NA_real_, diff_unemp_ts), naive_diff = lag(c(NA_real_, diff_unemp_ts)))
original_ts$lag_original <- original_ts$core - original_ts$diff
original_ts$lag_naive <- original_ts$core - original_ts$naive_diff

# Import and undifference -----------------------------------------
#So all I have to do is bring in the predicted values from this, and run them backwards through the system
arima_forecast <- read_rds(paste0(export, "other_cases/unemp/arima_forecast.rds"))
forest_forecast <- read_rds(paste0(export, "other_cases/unemp/forecast.rds"))
ar1_forecast <- read_rds(paste0(export, "other_cases/unemp/ar1_forecast.rds"))
base_forecast <- read_rds(paste0(export, "other_cases/unemp/base_forecast.rds"))

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
detrend_arima_forecast <- ts(full_df$lag_ar1_forecast[-c(1:504)], start = c(1989, 12), frequency = 12)
detrend_forest_forecast <- ts(full_df$lag_ar1_forecast[-c(1:504)], start = c(1989, 12), frequency = 12)
detrend_ar1_forecast <- ts(full_df$lag_ar1_forecast[-c(1:504)], start = c(1989, 12), frequency = 12)
detrend_base_forecast <- ts(full_df$lag_ar1_forecast[-c(1:504)], start = c(1989, 12), frequency = 12)
detrend_naive_forecast <- ts(original_ts$lag_naive[-c(1:2)], start = c(1948, 2), frequency = 12)

#Detrend them to get the original forecast
fitted <- trend_model$fitted.values
fitted_ts <- ts(fitted, start = c(1948, 1), frequency = 12)
this_fitted_ts <- window(fitted_ts, start = c(1989, 12), end = c(1999, 12))

arima_forecast <- this_fitted_ts + detrend_arima_forecast
forest_forecast <- this_fitted_ts + detrend_forest_forecast
ar1_forecast <- this_fitted_ts + detrend_ar1_forecast
base_forecast <- this_fitted_ts + detrend_base_forecast
naive_forecast <- this_fitted_ts + detrend_naive_forecast

# Compare values ---------------------------
accuracy(unemp_ts, arima_forecast)
accuracy(unemp_ts, forest_forecast)
accuracy(unemp_ts, ar1_forecast)
accuracy(unemp_ts, base_forecast)
accuracy(unemp_ts, naive_forecast)

# Export ------------------------------
write_rds(arima_forecast, paste0(export, "other_cases/unemp/retransformed_forecasts/arima_forecast.rds"))
write_rds(forest_forecast, paste0(export, "other_cases/unemp/retransformed_forecasts/forest_forecast.rds"))
write_rds(ar1_forecast, paste0(export, "other_cases/unemp/retransformed_forecasts/ar1_forecast.rds"))
write_rds(base_forecast, paste0(export, "other_cases/unemp/retransformed_forecasts/base_forecast.rds"))
write_rds(naive_forecast, paste0(export, "other_cases/unemp/retransformed_forecasts/naive_forecast.rds"))
