# Header ---------------------------------------
rm(list=ls())
header <- source("header.R")

#Code ------------------------------------------
df <- read.csv(paste0(import, "UKCPI.csv"))
arima_forecast <- read_rds(paste0(export, "other_cases/uk_inflation/arima_forecast.rds"))
forest_forecast <- read_rds(paste0(export, "other_cases/uk_inflation/forecast.rds"))
ar1_forecast <- read_rds(paste0(export, "other_cases/uk_inflation/ar1_forecast.rds"))
base_forecast <- read_rds(paste0(export, "other_cases/uk_inflation/base_forecast.rds"))

values_df <- df[-c(1:172),]
colnames(values_df) <- c("date", "cpi")
values_df$date <- seq(as.Date("1988/1/1"), as.Date("2021/1/1"), "month")
values_df$cpi <- as.numeric(values_df$cpi)
values_df <- values_df %>% 
  mutate(l_cpi = log(cpi),
         l_cpi_1 = dplyr::lag(l_cpi),
         infl = l_cpi - l_cpi_1) %>% 
  dplyr::select(date, infl)
values_df <- values_df[-1,]
rownames(values_df) <- seq(1, nrow(values_df))
tsData <- ts(values_df$infl, start = c(1988, 2), frequency = 12)

infl <- window(tsData, start = c(2010, 1), end = c(2020, 1))

naive_forecast <- window(tsData, start = c(2009, 12), end = c(2019, 12))
naive <- ts(naive_forecast, start = c(2010, 1), frequency = 12)

graph_df <- data.frame(date = seq(as.Date("2010/1/1"), as.Date("2020/1/1"), "month")) %>% 
  dplyr::mutate(arima = arima_forecast,
                forest = forest_forecast,
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
