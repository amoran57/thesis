# Header ---------------------------------------
rm(list=ls()) 
header <- source("header.R")

#Code ------------------------------------------
df <- read_rds(paste0(export, "master_data.rds")) 

values_df <- df %>% 
  filter(year >= 1962) %>% 
  dplyr::select(date, infl, rate10yr, rate3month, unemp, nat_unemp, year)


tsData <- ts(values_df$infl, start = c(1962,1), frequency = 12)

#Set model ----------------------------------
monthly_dates <- seq(as.Date("1999/1/1"), as.Date("2020/1/1"), "month")
var_pred <- c()

tic("var")
for (k in 1:length(monthly_dates)) {
  monthx <- monthly_dates[k]
  df_train <- values_df %>% 
    filter(date <= monthx) %>% 
    dplyr::select(-date, -year)
  
  df_train_ts <- ts(df_train, start = c(1962, 1), frequency = 12)
  
  model <- VAR(df_train_ts, lag.max = 6, ic = "AIC", type = "both")
  
  prediction_list <- predict(model, n.ahead = 1)
  forecast_list <- prediction_list$fcst
  prediction <- as.data.frame(forecast_list) %>% 
    dplyr::select(infl = infl.fcst)
  
  var_pred[k] <- prediction[1,]
}
toc()

var_ts <- ts(var_pred, start = c(1999,1), frequency = 12)

#export ------------------------------------------
write_rds(var_ts, paste0(export,"multivariate/var_forecast.rds"))


