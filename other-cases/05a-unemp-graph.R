# Header ---------------------------------------
rm(list=ls())
header <- source("header.R")

#Code ------------------------------------------
us_df <- read_rds(paste0(export, "master_data.rds"))
df <- read_rds(paste0(export, "other_cases/built_data.rds"))
arima_forecast <- read_rds(paste0(export, "other_cases/unemp/retransformed_forecasts/arima_forecast.rds"))
forest_forecast <- read_rds(paste0(export, "other_cases/unemp/retransformed_forecasts/forest_forecast.rds"))
ar1_forecast <- read_rds(paste0(export, "other_cases/unemp/retransformed_forecasts/ar1_forecast.rds"))
base_forecast <- read_rds(paste0(export, "other_cases/unemp/retransformed_forecasts/base_forecast.rds"))
naive_forecast <- read_rds(paste0(export, "other_cases/unemp/retransformed_forecasts/naive_forecast.rds"))
us_values <- us_df %>% 
  select(date, unemp)

tsData <- ts(us_values$unemp[-c(1:12)], start = c(1948, 1), frequency = 12)
unemp <- window(tsData, start = c(1990, 1), end = c(2000, 1))

this_naive_forecast <- window(tsData, start = c(1989, 12), end = c(1999, 12))
naive <- ts(naive_forecast, start = c(1990, 1), frequency = 12)

graph_df <- data.frame(date = seq(as.Date("1990/1/1"), as.Date("2000/1/1"), "month")) %>% 
  dplyr::mutate(arima = arima_forecast,
                forest = forest_forecast,
                unemp = unemp)


tidy_graph <- gather(data = graph_df, key = "key", value = "value", "arima":"unemp")

plot <- ggplot(data = tidy_graph, aes(x = date, y = value, color = key)) +
  geom_line()
plot

accuracy(tsData, arima_forecast)
accuracy(tsData, forest_forecast)
accuracy(tsData, ar1_forecast)
accuracy(tsData, base_forecast)
accuracy(tsData, naive_forecast)
accuracy(tsData, naive)
