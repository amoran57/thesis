# Header ---------------------------------------
rm(list=ls())
header <- source("header.R")

#Code ------------------------------------------
df <- read_rds(paste0(export, "master_data.rds"))
arima_forecast <- read_rds(paste0(export, "other_cases/unemp/arima_forecast.rds"))
forest_forecast <- read_rds(paste0(export, "other_cases/unemp/forecast.rds"))

values_df <- df %>% 
  dplyr::filter(year >= 1949)

tsData <- ts(values_df$unemp, start = c(1949, 1), frequency = 12)
unemp <- window(tsData, start = c(1990, 1), end = c(2000, 1))

naive_forecast <- window(tsData, start = c(1989, 12), end = c(1999, 12))
naive <- ts(naive_forecast, start = c(1990, 1), frequency = 12)

graph_df <- data.frame(date = seq(as.Date("1990/1/1"), as.Date("2000/1/1"), "month")) %>% 
  dplyr::mutate(arima = arima_forecast,
                forest = forest_forecast,
                unemp = unemp)


tidy_graph <- gather(data = graph_df, key = "key", value = "value", "arima":"unemp")

plot <- ggplot(data = tidy_graph, aes(x = date, y = value, color = key)) +
  geom_line()
plot




