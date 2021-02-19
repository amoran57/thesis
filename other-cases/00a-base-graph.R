# Header ---------------------------------------
rm(list=ls())
header <- source("header.R")

#Code ------------------------------------------
df <- read_rds(paste0(export, "master_data.rds"))
arima_forecast <- read_rds(paste0(export, "other_cases/arima_forecast.rds"))
forest_forecast <- read_rds(paste0(export, "other_cases/forest_forecast.rds"))

values_df <- df %>% 
  dplyr::filter(year >= 1959)

tsData <- ts(values_df$infl, start = c(1959, 1), frequency = 12)
infl <- window(tsData, start = c(1999, 1), end = c(2020, 1))

naive_forecast <- window(tsData, start = c(1998, 12), end = c(2019, 12))
naive <- ts(naive_forecast, start = c(1999, 1), frequency = 12)

graph_df <- data.frame(date = seq(as.Date("1999/1/1"), as.Date("2020/1/1"), "month")) %>% 
  dplyr::mutate(arima = arima_forecast,
                forest = forest_forecast)


tidy_graph <- gather(data = graph_df, key = "key", value = "value", "arima":"infl")

plot <- ggplot(data = tidy_graph, aes(x = date, y = value, color = key)) +
  geom_line()
plot

# Compare sample vs regular and ARIMA ---------------
count(graph_df, closer)
count(graph_df, mean_closer)
count(graph_df, sample_closer)
count(graph_df, ar1_mean_closer)
count(graph_df, any_closer)
count(graph_df, strict_closer)

accuracy(tsData, ar1_ts)
accuracy(tsData, mean)
accuracy(tsData, ar1_sample_ts)
accuracy(tsData, ar1_mean)
accuracy(tsData, adjusted)
accuracy(tsData, arima_forecast)
accuracy(tsData, strict_ar1)
accuracy(tsData, naive)

mean(infl)
mean(ar1_ts)
mean(ar1_sample_ts)
mean(ar1_mean)
mean(arima_forecast)


