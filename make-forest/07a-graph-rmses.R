# Header ---------------------------------------
rm(list=ls())
header <- source("header.R")

#Code ------------------------------------------
df <- read_rds(paste0(export, "master_data.rds"))
arima_forecast <- read_rds(paste0(export, "20_year_forecasts/arima_forecast.rds"))
forest_forecast <- read_rds(paste0(export, "20_year_forecasts/ar1_forecast.rds"))
forest_no_sample_forecast <- read_rds(paste0(export, "20_year_forecasts/ar1_forecast_no_sample.rds"))
strict_ar1_forecast <- read_rds(paste0(export, "20_year_forecasts/strict_ar1_forecast.rds"))
mean_forecast <- read_rds(paste0(export, "20_year_forecasts/mean_forecast_sample.rds"))

values_df <- df %>% 
  dplyr::filter(year >= 1959)

tsData <- ts(values_df$infl, start = c(1959, 1), frequency = 12)
infl <- window(tsData, start = c(1999, 1), end = c(2020, 1))

naive_forecast <- window(tsData, start = c(1998, 12), end = c(2019, 12))
naive <- ts(naive_forecast, start = c(1999, 1), frequency = 12)

graph_df <- data.frame(date = seq(as.Date("1999/1/1"), as.Date("2020/1/1"), "month")) %>% 
  dplyr::mutate(arima = arima_forecast,
                forest = forest_forecast,
                mean = mean_forecast,
                ar1 = strict_ar1_forecast,
                naive = naive,
                infl = infl)



tidy_graph <- gather(data = graph_df, key = "key", value = "value", "arima":"infl")

plot <- ggplot(data = tidy_graph, aes(x = date, y = value, color = key)) +
  geom_line()
plot

# Compare sample vs regular and ARIMA ---------------
accuracy(tsData, arima_forecast)
accuracy(tsData, forest_forecast)
accuracy(tsData, forest_no_sample_forecast)
accuracy(tsData, strict_ar1_forecast)
accuracy(tsData, mean_forecast)

mean(infl)
mean(arima_forecast)
mean(forest_forecast)
mean(forest_no_sample_forecast, na.rm = T)
mean(strict_ar1_forecast)
mean(mean_forecast)


