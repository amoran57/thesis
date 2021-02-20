# Header ---------------------------------------
rm(list=ls())
header <- source("header.R")

#Code ------------------------------------------
df <- read_rds(paste0(export, "master_data.rds"))
arima_3 <- read_rds(paste0(export, "different_horizons/horizon3month/arima_forecast.rds"))
forest_3 <- read_rds(paste0(export, "different_horizons/horizon3month/forecast.rds"))
arima_6 <- read_rds(paste0(export, "different_horizons/horizon6month/arima_forecast.rds"))
forest_6 <- read_rds(paste0(export, "different_horizons/horizon6month/forecast.rds"))
arima_12 <- read_rds(paste0(export, "different_horizons/horizon12month/arima_forecast.rds"))
forest_12 <- read_rds(paste0(export, "different_horizons/horizon12month/forecast.rds"))

values_df <- df %>% 
  dplyr::filter(year >= 1959)

tsData <- ts(values_df$infl, start = c(1959, 1), frequency = 12)
infl <- window(tsData, start = c(1999, 1), end = c(2020, 1))

naive_forecast <- window(tsData, start = c(1998, 12), end = c(2019, 12))
naive <- ts(naive_forecast, start = c(1999, 1), frequency = 12)

#Analysis
accuracy(tsData, arima_3)
accuracy(tsData, forest_3)
accuracy(tsData, arima_6)
accuracy(tsData, forest_6)
accuracy(tsData, arima_12)
accuracy(tsData, forest_12)
accuracy(tsData, naive)

mean(infl)
mean(arima_3)
mean(forest_3)
mean(arima_6)
mean(forest_6)
mean(arima_12)
mean(forest_12)

#Graph
graph_df <- data.frame(date = seq(as.Date("1999/1/1"), as.Date("2020/1/1"), "month")) %>% 
  dplyr::mutate(arima_3 = arima_3,
                arima_6 = arima_6,
                arima_12 = arima_12,
                forest_3 = forest_3,
                forest_6 = forest_6,
                forest_12 = forest_12,
                naive = naive,
                infl = infl)

tidy_graph <- gather(data = graph_df, key = "key", value = "value", "arima_3":"infl")

plot <- ggplot(data = tidy_graph, aes(x = date, y = value, color = key)) +
  geom_line()
plot

