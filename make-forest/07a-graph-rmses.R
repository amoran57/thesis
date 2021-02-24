# Header ---------------------------------------
rm(list=ls())
header <- source("header.R")

#Code ------------------------------------------
df <- read_rds(paste0(export, "master_data.rds"))
arima_forecast <- read_rds(paste0(export, "20_year_forecasts/arima_forecast.rds"))
forest_forecast <- read_rds(paste0(export, "20_year_forecasts/ar1_forecast.rds"))
forest_no_sample_forecast <- read_rds(paste0(export, "20_year_forecasts/ar1_forecast_no_sample.rds"))
strict_ar1_forecast <- read_rds(paste0(export, "20_year_forecasts/strict_ar1_forecast.rds"))
mean_forecast <- read_rds(paste0(export, "20_year_forecasts/mean_forecast_sample.rds"))

values_df <- df %>% 
  dplyr::filter(year >= 1959)

tsData <- ts(values_df$infl, start = c(1959, 1), frequency = 12)
infl <- window(tsData, start = c(1999, 1), end = c(2020, 1))

naive_forecast <- window(tsData, start = c(1998, 12), end = c(2019, 12))
naive <- ts(naive_forecast, start = c(1999, 1), frequency = 12)

# Get errors for each -----------------------------
rmses_df <- data.frame(date = seq(as.Date("1999/1/1"), as.Date("2020/1/1"), "month"))
rmses_df$arima <- arima_forecast - infl
rmses_df$forest <- forest_forecast - infl
rmses_df$mean <- mean_forecast - infl
rmses_df$ar1 <- strict_ar1_forecast - infl
rmses_df$naive <- naive - infl

t_rmses <- data.frame(t(rmses_df[,-1]))
colnames(t_rmses) <- rmses_df$date

#Get rolling average df ---------------------------
horizon <- 12
avg_rmses <- data.frame(matrix(nrow = nrow(t_rmses), ncol = ncol(t_rmses) - horizon))
rownames(avg_rmses) <- rownames(t_rmses)
colnames(avg_rmses) <- rmses_df$date[-c(1:12)]
for(i in horizon:(ncol(avg_rmses) + horizon)) {
  small_df <- t_rmses[,c((i-horizon+1):i)]
  small_sqd <- small_df^2
  mses <- rowMeans(small_sqd, na.rm = TRUE)
  rmses <- mses^0.5
  avg_rmses[,(i-horizon+1)] <- rmses
}

# Graph results ------------------------------
graph_df <- data.frame(t(avg_rmses))
graph_df$date <- as.Date(rownames(graph_df))
graph_df <- graph_df %>% 
  select(date, everything())

tidy_graph <- gather(data = graph_df, key = "key", value = "value", "arima":"naive")

plot <- ggplot(data = tidy_graph, aes(x = date, y = value, color = key)) +
  geom_line()
plot


#Ger cumulative RMSE df ---------------------------
horizon <- 12
cum_rmses <- data.frame(matrix(nrow = nrow(t_rmses), ncol = ncol(t_rmses) - horizon))
rownames(cum_rmses) <- rownames(t_rmses)
colnames(cum_rmses) <- rmses_df$date[-c(1:12)]
for(i in horizon:(ncol(cum_rmses) + horizon)) {
  small_df <- t_rmses[,c(1:i)]
  small_sqd <- small_df^2
  mses <- rowMeans(small_sqd, na.rm = TRUE)
  rmses <- mses^0.5
  cum_rmses[,(i-horizon+1)] <- rmses
}

# Graph results ------------------------------
cum_graph_df <- data.frame(t(cum_rmses))
cum_graph_df$date <- as.Date(rownames(cum_graph_df))
cum_graph_df <- cum_graph_df %>% 
  select(date, everything())

cum_tidy_graph <- gather(data = cum_graph_df, key = "key", value = "value", "arima":"naive")

cum_plot <- ggplot(data = cum_tidy_graph, aes(x = date, y = value, color = key)) +
  geom_line()
cum_plot


write.csv(graph_df, paste0(export, "rmse_graphs/avg_rmses.csv"))
write.csv(cum_graph_df, paste0(export, "rmse_graphs/cum_rmses.csv"))
