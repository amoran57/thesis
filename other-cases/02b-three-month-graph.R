# Header ---------------------------------------
rm(list=ls())
header <- source("header.R")

#Code ------------------------------------------
df <- read_rds(paste0(export, "other_cases/built_data.rds"))
arima_forecast <- read_rds(paste0(export, "other_cases/rate3month/arima_forecast.rds"))
forest_forecast <- read_rds(paste0(export, "other_cases/rate3month/forecast.rds"))
ar1_forecast <- read_rds(paste0(export, "other_cases/rate3month/ar1_forecast.rds"))
base_forecast <- read_rds(paste0(export, "other_cases/rate3month/base_forecast.rds"))

values_df <- df %>% 
  dplyr::filter(lubridate::year(date) >= 1948)

tsData <- ts(values_df$rate3month, start = c(1948, 1), frequency = 12)
rate3month <- window(tsData, start = c(1985, 1), end = c(1995, 1))

naive_forecast <- window(tsData, start = c(1984, 12), end = c(1994, 12))
naive <- ts(naive_forecast, start = c(1985, 1), frequency = 12)

graph_df <- data.frame(date = seq(as.Date("1985/1/1"), as.Date("1995/1/1"), "month")) %>% 
  dplyr::mutate(arima = arima_forecast,
                forest = forest_forecast,
                rate3month = rate3month)


tidy_graph <- gather(data = graph_df, key = "key", value = "value", "arima":"rate3month")

plot <- ggplot(data = tidy_graph, aes(x = date, y = value, color = key)) +
  geom_line()
plot

accuracy(tsData, arima_forecast)
accuracy(tsData, forest_forecast)
accuracy(tsData, ar1_forecast)
accuracy(tsData, base_forecast)
accuracy(tsData, naive)