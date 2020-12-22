# Header ---------------------------------------
rm(list=ls())
header <- source("header.R")

#Code ------------------------------------------
df <- read_rds(paste0(export, "master_data.rds"))
ar1_pred_forecast <- read_rds(paste0(export, "4_year_forecasts/ar1_forecast.rds"))
arima_forecast <- read_rds(paste0(export, "4_year_forecasts/arima_forecast.rds"))
ar1_real_forecast <- read_rds(paste0(export, "4_year_forecasts/ar1_obj_forecast.rds"))
ar1_fat_leaf_forecast <- read_rds(paste0(export, "4_year_forecasts/ar1_obj_forecast_2.rds"))
arima_forecast <- ts(arima_forecast, start = c(1999, 1), frequency = 12)
ar1_fat_leaf_forecast <- ts(ar1_fat_leaf_forecast, start = c(1999, 1), frequency = 12)

values_df <- df %>% 
  dplyr::filter(year >= 1959)

tsData <- ts(values_df$infl, start = c(1959, 1), frequency = 12)
infl <- window(tsData, start = c(1999, 1), end = c(2020, 1))

naive_forecast <- window(tsData, start = c(1998, 12), end = c(2019, 12))
naive <- ts(naive_forecast, start = c(1999, 1), frequency = 12)

graph_df <- data.frame(date = seq(as.Date("1999/1/1"), as.Date("2020/1/1"), "month")) %>% 
  dplyr::mutate(arima = arima_forecast,
                ar1 = ar1_fat_leaf_forecast,
                infl = infl)

graph_df$ar1_diff <- graph_df$ar1 - graph_df$infl
graph_df$ar1_fat_diff <- graph_df$ar1_fat - graph_df$infl
graph_df$arima_diff <- graph_df$arima - graph_df$infl
graph_df$closer <- ifelse(abs(graph_df$ar1_diff) < abs(graph_df$arima_diff), "ar1", "arima")
graph_df$closer_fat <- ifelse(abs(graph_df$ar1_fat_diff) < abs(graph_df$arima_diff), "ar1", "arima")
graph_df$closer <- as.factor(graph_df$closer)
graph_df$closer_fat <- as.factor(graph_df$closer_fat)

tidy_graph <- gather(data = graph_df, key = "key", value = "value", "arima":"infl")

plot <- ggplot(data = tidy_graph, aes(x = date, y = value, color = key)) +
  geom_line()
plot

count(graph_df, closer)
count(graph_df, closer_fat)
tampered_ar1_ts <- ts(graph_df$ar1, start = c(1999, 1), frequency = 12)
accuracy(tsData, tampered_ar1_ts)
accuracy(tsData, ar1_real_forecast)
accuracy(tsData, ar1_fat_leaf_forecast)
accuracy(tsData, arima_forecast)
accuracy(tsData, naive)
mean(infl)
mean(ar1_fat_leaf_forecast)
mean(arima_forecast)
