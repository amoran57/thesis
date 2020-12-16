# Header ---------------------------------------
rm(list=ls())
header <- source("header.R")

#Code ------------------------------------------
df <- read_rds(paste0(export, "master_data.rds"))
ar1_forecast <- read_rds(paste0(export, "4_year_forecasts/ar1_forecast.rds"))
arima_forecast <- read_rds(paste0(export, "4_year_forecasts/arima_forecast.rds"))
test_ar1 <- ar1_forecast*0.8

values_df <- df %>% 
  dplyr::filter(year >= 1959)

tsData <- ts(values_df$infl, start = c(1959, 1), frequency = 12)
infl <- window(tsData, start = c(1999, 1), end = c(2003, 1))

graph_df <- data.frame(date = seq(as.Date("1999/1/1"), as.Date("2003/1/1"), "month")) %>% 
  dplyr::mutate(arima = arima_forecast,
                ar1 = ar1_forecast,
                test = test_ar1,
                infl = infl)

tidy_graph <- gather(data = graph_df, key = "key", value = "value", "arima":"infl")

plot <- ggplot(data = tidy_graph, aes(x = date, y = value, color = key)) +
  geom_line()
plot
