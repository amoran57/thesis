# Header ---------------------------------------
rm(list=ls())
header <- source("header.R")

#Code ------------------------------------------
df <- read_rds(paste0(export, "master_data.rds"))
arima_3 <- read_rds(paste0(export, "different_horizons/horizon3month/arima_forecast.rds"))
forest_3 <- read_rds(paste0(export, "different_horizons/horizon3month/forecast.rds"))
base_3 <- read_rds(paste0(export, "different_horizons/horizon3month/base_forecast.rds"))
ar_3 <- read_rds(paste0(export, "different_horizons/horizon3month/ar1_forecast.rds"))
naive_3 <- read_rds(paste0(export, "different_horizons/horizon3month/naive_forecast.rds"))
arima_6 <- read_rds(paste0(export, "different_horizons/horizon6month/arima_forecast.rds"))
forest_6 <- read_rds(paste0(export, "different_horizons/horizon6month/forecast.rds"))
base_6 <- read_rds(paste0(export, "different_horizons/horizon6month/base_forecast.rds"))
ar_6 <- read_rds(paste0(export, "different_horizons/horizon6month/ar1_forecast.rds"))
naive_6 <- read_rds(paste0(export, "different_horizons/horizon6month/naive_forecast.rds"))
arima_12 <- read_rds(paste0(export, "different_horizons/horizon12month/arima_forecast.rds"))
forest_12 <- read_rds(paste0(export, "different_horizons/horizon12month/forecast.rds"))
base_12 <- read_rds(paste0(export, "different_horizons/horizon12month/base_forecast.rds"))
ar_12 <- read_rds(paste0(export, "different_horizons/horizon12month/ar1_forecast.rds"))
naive_12 <- read_rds(paste0(export, "different_horizons/horizon12month/naive_forecast.rds"))

values_df <- df %>% 
  dplyr::filter(year >= 1959)

tsData <- ts(values_df$infl, start = c(1959, 1), frequency = 12)
infl <- window(tsData, start = c(1999, 1), end = c(2020, 1))

#Analysis
accuracy(tsData, arima_3)
accuracy(tsData, forest_3)
accuracy(tsData, base_3)
accuracy(tsData, ar_3)
accuracy(tsData, naive_3)
accuracy(tsData, arima_6)
accuracy(tsData, forest_6)
accuracy(tsData, base_6)
accuracy(tsData, ar_6)
accuracy(tsData, naive_6)
accuracy(tsData, arima_12)
accuracy(tsData, forest_12)
accuracy(tsData, base_12)
accuracy(tsData, ar_12)
accuracy(tsData, naive_12)

mean(infl)
mean(arima_3)
mean(forest_3)
mean(base_3)
mean(ar_3)
mean(naive_3)
mean(arima_6)
mean(forest_6)
mean(base_6)
mean(ar_6)
mean(naive_6)
mean(arima_12)
mean(forest_12)
mean(base_12)
mean(ar_12)
mean(naive_12)

#Graph
graph_df <- data.frame(date = seq(as.Date("1999/1/1"), as.Date("2020/1/1"), "month")) %>% 
  dplyr::mutate(arima_3 = arima_3,
                arima_6 = arima_6,
                arima_12 = arima_12,
                forest_3 = forest_3,
                forest_6 = forest_6,
                forest_12 = forest_12,
                base_3 = base_3,
                base_6 = base_6,
                base_12 = base_12,
                ar_3 = ar_3,
                ar_6 = ar_6,
                ar_12 = ar_12,
                naive = naive,
                infl = infl)

tidy_graph <- gather(data = graph_df, key = "key", value = "value", "arima_3":"infl")

plot <- ggplot(data = tidy_graph, aes(x = date, y = value, color = key)) +
  geom_line()
plot

