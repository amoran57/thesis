# Header ---------------------------------------
rm(list=ls())
header <- source("header.R")

#Code ------------------------------------------
df <- read_rds(paste0(export, "master_data.rds"))
arima_forecast <- read_rds(paste0(export, "4_year_forecasts/arima_forecast.rds"))
ar1_fat_leaf_forecast <- read_rds(paste0(export, "4_year_forecasts/ar1_obj_forecast.rds"))
ar1_fat_leaf_sample <- read_rds(paste0(export, "4_year_forecasts/ar1_obj_forecast_sample.rds"))
strict_ar1 <- read_rds(paste0(export, "4_year_forecasts/strict_ar1_forecast.rds"))
arima_forecast <- ts(arima_forecast, start = c(1999, 1), frequency = 12)
ar1_ts <- ts(ar1_fat_leaf_forecast, start = c(1999, 1), frequency = 12)
ar1_sample_ts <- ts(ar1_fat_leaf_sample, start = c(1999,1), frequency = 12)
ar1_mean <- (ar1_ts + ar1_sample_ts)/2

values_df <- df %>% 
  dplyr::filter(year >= 1959)

tsData <- ts(values_df$infl, start = c(1959, 1), frequency = 12)
infl <- window(tsData, start = c(1999, 1), end = c(2020, 1))

naive_forecast <- window(tsData, start = c(1998, 12), end = c(2019, 12))
naive <- ts(naive_forecast, start = c(1999, 1), frequency = 12)

graph_df <- data.frame(date = seq(as.Date("1999/1/1"), as.Date("2020/1/1"), "month")) %>% 
  dplyr::mutate(arima = arima_forecast,
                # ar1 = ar1_fat_leaf_forecast,
                sample = ar1_fat_leaf_sample,
                # mean = ar1_mean,
                strict_ar1 = strict_ar1,
                infl = infl)

graph_df$ar1_diff <- graph_df$ar1 - graph_df$infl
graph_df$arima_diff <- graph_df$arima - graph_df$infl
graph_df$ar1_sample_diff <- graph_df$sample - graph_df$infl
graph_df$ar1_mean_diff <- ar1_mean - graph_df$infl
graph_df$strict_diff <- graph_df$strict_ar1 - graph_df$infl
graph_df$closer <- ifelse(abs(graph_df$ar1_diff) < abs(graph_df$arima_diff), "ar1", "arima")
graph_df$closer <- as.factor(graph_df$closer)
graph_df$sample_closer <- ifelse(abs(graph_df$ar1_sample_diff) < abs(graph_df$arima_diff), "ar1", "arima")
graph_df$sample_closer <- as.factor(graph_df$sample_closer)
graph_df$mean_closer <- ifelse(abs(graph_df$ar1_mean_diff) < abs(graph_df$arima_diff), "ar1", "arima")
graph_df$mean_closer <- as.factor(graph_df$mean_closer)
graph_df$strict_closer <- ifelse(abs(graph_df$ar1_sample_diff) < abs(graph_df$strict_diff), "ar1", "arima")
graph_df$strict_closer <- as.factor(graph_df$strict_closer)
graph_df$any_closer <- ifelse(graph_df$closer == "ar1" | graph_df$sample_closer == "ar1", "ar1", "arima")
graph_df$any_closer <- as.factor(graph_df$any_closer)

tidy_graph <- gather(data = graph_df, key = "key", value = "value", "arima":"infl")

plot <- ggplot(data = tidy_graph, aes(x = date, y = value, color = key)) +
  geom_line()
plot

# Compare sample vs regular and ARIMA ---------------
count(graph_df, closer)
count(graph_df, sample_closer)
count(graph_df, mean_closer)
count(graph_df, any_closer)
count(graph_df, strict_closer)

accuracy(tsData, ar1_ts)
accuracy(tsData, ar1_sample_ts)
accuracy(tsData, arima_forecast)
accuracy(tsData, naive)

mean(infl)
mean(ar1_ts)
mean(ar1_sample_ts)
mean(arima_forecast)
