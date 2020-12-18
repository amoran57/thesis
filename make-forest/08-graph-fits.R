# Header ---------------------------------------
rm(list=ls())
header <- source("header.R")

#Code ------------------------------------------
df <- read_rds(paste0(export, "master_data.rds"))
bayes_ar1 <- read_rds(paste0(export, "bayes_forest_ar1_fit.rds"))
grid_ar1 <- read_rds(paste0(export, "grid_forest_ar1_fit.rds"))

values_df <- df %>% 
  dplyr::filter(year >= 1959)

tsData <- ts(values_df$infl, start = c(1959, 1), frequency = 12)
infl <- window(tsData, start = c(1959, 12))
arima_fit <- ts(auto.arima(tsData)$fitted[-c(1:11)], start = c(1959, 12), frequency = 12)

graph_df <- data.frame(date = seq(as.Date("1959/12/1"), as.Date("2020/8/1"), "month")) %>% 
  dplyr::mutate(arima = arima_fit,
                grid_ar1 = grid_ar1,
                bayes_ar1 = bayes_ar1,
                infl = infl)

tidy_graph <- gather(data = graph_df, key = "key", value = "value", "arima":"infl")

plot <- ggplot(data = tidy_graph, aes(x = date, y = value, color = key)) +
  geom_line()
plot


accuracy(tsData, bayes_ar1)
accuracy(tsData, grid_ar1)
accuracy(tsData, arima_fit)
