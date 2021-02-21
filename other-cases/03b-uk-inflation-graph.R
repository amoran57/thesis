# Header ---------------------------------------
rm(list=ls())
header <- source("header.R")

#Code ------------------------------------------
df <- read_rds(paste0(export, "other_cases/built_data.rds"))
arima_forecast <- read_rds(paste0(export, "other_cases/uk_inflation/arima_forecast.rds"))
forest_forecast <- read_rds(paste0(export, "other_cases/uk_inflation/forecast.rds"))
ar1_forecast <- read_rds(paste0(export, "other_cases/uk_inflation/ar1_forecast.rds"))
base_forecast <- read_rds(paste0(export, "other_cases/uk_inflation/base_forecast.rds"))

values_df <- df %>% 
  dplyr::filter(lubridate::year(date) >= 1988)
values_df <- values_df[-1,]
rownames(values_df) <- seq(1, nrow(values_df))
tsData <- ts(values_df$infl, start = c(1988, 2), frequency = 12)

infl <- window(tsData, start = c(2015, 1), end = c(2020, 1))

naive_forecast <- window(tsData, start = c(2009, 12), end = c(2019, 12))
naive <- ts(naive_forecast, start = c(2010, 1), frequency = 12)

graph_df <- data.frame(date = seq(as.Date("2015/1/1"), as.Date("2020/1/1"), "month")) %>% 
  dplyr::mutate(arima = arima_forecast[-c(1:60)],
                forest = forest_forecast[-c(1:60)],
                infl = infl)


tidy_graph <- gather(data = graph_df, key = "key", value = "value", "arima":"infl")

plot <- ggplot(data = tidy_graph, aes(x = date, y = value, color = key)) +
  geom_line()
plot

accuracy(tsData, arima_forecast)
accuracy(tsData, forest_forecast)
accuracy(tsData, ar1_forecast)
accuracy(tsData, base_forecast)
accuracy(tsData, naive)

accuracy(infl, arima_forecast)
accuracy(infl, forest_forecast)
accuracy(infl, ar1_forecast)
accuracy(infl, base_forecast)
accuracy(infl, naive)
