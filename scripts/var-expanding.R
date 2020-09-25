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
all_forecasts <- seq(as.Date("1999/4/1"), as.Date("2020/1/1"), "month")
all_forecasts <- as.data.frame(all_forecasts)
names(all_forecasts) <- c("date")
horizons <- c(3, 6, 12)

tic("var")
for (horizon in horizons) {
  var_pred <- c()
for (monthx in monthly_dates) {
  df_train <- values_df %>% 
    filter(date <= monthx) %>% 
    dplyr::select(-date, -year)
  
  df_train_ts <- ts(df_train, start = c(1962, 1), frequency = 12)
  
  model <- VAR(df_train_ts, lag.max = 6, ic = "AIC", type = "both")
  
  
  prediction <- predict(model, n.ahead = horizon)
  prediction <- prediction$fcst
  prediction <- as.data.frame(prediction) %>% 
    dplyr::select(infl = infl.fcst)
  value <- prediction[horizon,]
  
  var_pred <- c(var_pred, value)
}
  
  var_pred <- as.data.frame(var_pred)
  names(var_pred) <- c("prediction")
  if (horizon == 3) {
    var_pred <- var_pred %>% 
      dplyr::mutate(date = seq(as.Date("1999-04-01"), as.Date("2019-04-01"), "month")) %>% 
      dplyr::select(var_month3 = prediction, date)
  } else if (horizon == 6) {
    var_pred <- var_pred %>% 
      dplyr::mutate(date = seq(as.Date("1999-07-01"), as.Date("2019-07-01"), "month")) %>% 
      dplyr::select(var_month6 = prediction, date)
  } else if (horizon == 12) {
    var_pred <- var_pred %>% 
      dplyr::mutate(date = seq(as.Date("2000-01-01"), as.Date("2020-01-01"), "month")) %>% 
      dplyr::select(var_month12 = prediction, date)
  }
  
  all_forecasts <- dplyr::left_join(all_forecasts, var_pred, by = "date")
  
}
toc()


#export ------------------------------------------
write_rds(all_forecasts, paste0(export,"var_expanding_horizon.rds"))


