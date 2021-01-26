# Header ---------------------------------------
rm(list=ls())
header <- source("header.R")

#Code ------------------------------------------
df <- read_rds(paste0(export, "master_data.rds"))
arima_forecast <- read_rds(paste0(export, "4_year_forecasts/arima_forecast.rds"))
ar1_fat_leaf_forecast <- read_rds(paste0(export, "4_year_forecasts/ar1_obj_forecast.rds"))
ar1_fat_leaf_sample <- read_rds(paste0(export, "4_year_forecasts/ar1_obj_forecast_sample.rds"))
strict_ar1 <- read_rds(paste0(export, "4_year_forecasts/strict_ar1_forecast.rds"))
arima_forecast <- ts(arima_forecast, start = c(1999, 1), frequency = 12)
ar1_ts <- ts(ar1_fat_leaf_forecast, start = c(1999, 1), frequency = 12)
ar1_sample_ts <- ts(ar1_fat_leaf_sample, start = c(1999,1), frequency = 12)
ar1_mean <- (ar1_ts + ar1_sample_ts)/2

values_df <- df %>% 
  dplyr::filter(year >= 1959)

tsData <- ts(values_df$infl, start = c(1959, 1), frequency = 12)
infl <- window(tsData, start = c(1999, 1), end = c(2020, 1))

naive_forecast <- window(tsData, start = c(1998, 12), end = c(2019, 12))
naive <- ts(naive_forecast, start = c(1999, 1), frequency = 12)

graph_df <- data.frame(date = seq(as.Date("1999/1/1"), as.Date("2020/1/1"), "month")) %>% 
  dplyr::mutate(arima = arima_forecast,
                ar1 = ar1_fat_leaf_forecast,
                sample = ar1_fat_leaf_sample,
                mean = ar1_mean,
                strict_ar1 = strict_ar1,
                naive = naive,
                infl = infl)

graph_df$ar1_diff <- graph_df$ar1 - graph_df$infl
graph_df$arima_diff <- graph_df$arima - graph_df$infl
graph_df$ar1_sample_diff <- graph_df$sample - graph_df$infl
graph_df$ar1_mean_diff <- ar1_mean - graph_df$infl
graph_df$strict_diff <- graph_df$strict_ar1 - graph_df$infl
graph_df$closer <- ifelse(abs(graph_df$ar1_diff) < abs(graph_df$arima_diff), "ar1", "arima")
graph_df$closer <- as.factor(graph_df$closer)
graph_df$sample_closer <- ifelse(abs(graph_df$ar1_sample_diff) < abs(graph_df$arima_diff), "ar1", "arima")
graph_df$sample_closer <- as.factor(graph_df$sample_closer)
graph_df$mean_closer <- ifelse(abs(graph_df$ar1_mean_diff) < abs(graph_df$arima_diff), "ar1", "arima")
graph_df$mean_closer <- as.factor(graph_df$mean_closer)
graph_df$strict_closer <- ifelse(abs(graph_df$ar1_sample_diff) < abs(graph_df$strict_diff), "ar1", "arima")
graph_df$strict_closer <- as.factor(graph_df$strict_closer)
graph_df$any_closer <- ifelse(graph_df$closer == "ar1" | graph_df$sample_closer == "ar1", "ar1", "arima")
graph_df$any_closer <- as.factor(graph_df$any_closer)

tidy_graph <- gather(data = graph_df, key = "key", value = "value", "arima":"infl")

plot <- ggplot(data = tidy_graph, aes(x = date, y = value, color = key)) +
  geom_line()
plot

# Compare sample vs regular and ARIMA ---------------
count(graph_df, closer)
count(graph_df, sample_closer)
count(graph_df, mean_closer)
count(graph_df, any_closer)
count(graph_df, strict_closer)

accuracy(tsData, ar1_ts)
accuracy(tsData, ar1_sample_ts)
accuracy(tsData, ar1_mean)
accuracy(tsData, arima_forecast)
accuracy(tsData, strict_ar1)
accuracy(tsData, naive)

mean(infl)
mean(ar1_ts)
mean(ar1_sample_ts)
mean(ar1_mean)
mean(arima_forecast)

#Compare mean of forests to ARIMA ------------------------------
mean_better <- graph_df %>% 
  filter(mean_closer == "ar1") %>% 
  select(date, arima, mean, infl, arima_diff, ar1_mean_diff)

arima_better <- graph_df %>% 
  filter(mean_closer == "arima") %>% 
  select(date, arima, mean, infl, arima_diff, ar1_mean_diff)

mean_tidy <- gather(mean_better, key = "key", value = "value", "arima_diff":"ar1_mean_diff")
arima_tidy <- gather(arima_better, key = "key", value = "value", "arima_diff":"ar1_mean_diff")
overall_tidy <- gather(graph_df, key = "key", value = "value", "arima_diff", "ar1_mean_diff")

mean_plot <- ggplot(mean_tidy, aes(x = value, color = key)) +
  # geom_histogram(binwidth = 0.002) +
  geom_density(aes(y=0.002*..count..)) 
mean_plot

arima_plot <- ggplot(arima_tidy, aes(x = value, y = 0.5*..count.., color = key)) +
  # geom_histogram(binwidth = 0.002) +
  geom_density(aes(y=0.002*..count..))
arima_plot

overall_plot <- ggplot(overall_tidy, aes(x = value, y = 0.5*..count.., color = key)) +
  # geom_histogram(binwidth = 0.002) +
  geom_density(aes(y=0.002*..count..)) 
overall_plot

grid.arrange(mean_plot, arima_plot, overall_plot, nrow = 3, ncol = 1)

hist(x = mean_better$ar1_mean_diff, freq = TRUE)
lines(x = density(x = mean_better$ar1_mean_diff), col = "red")


# Generate 4-part graph ----------------------------------------
myblanktheme <- theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15)), 
                      legend.title = element_blank(), 
                      legend.text = element_blank(), 
                      axis.title = element_text(),
                      axis.text = element_text())

tidy_1 <- gather(graph_df, key = "key", value = "value", "infl", "arima")
tidy_1$key <- ifelse(tidy_1$key == "arima", "zarima", "infl")
plot_1 <- ggplot(tidy_1, aes(x = date, y = value, color = key)) +
  geom_line() +
  scale_color_manual(values = c("#000000", "#5F5F5F")) +
  labs(title = "ARIMA model", x = "", y = "") +
  myblanktheme

tidy_2 <- gather(graph_df, key = "key", value = "value", "sample", "infl")
plot_2 <- ggplot(tidy_2, aes(x = date, y = value, color = key)) +
  geom_line() +
  scale_color_manual(values = c("#000000", "#5F5F5F")) +
  ggtitle("Random Forest model") +
  myblanktheme

tidy_3 <- gather(graph_df, key = "key", value = "value", "mean", "infl")
plot_3 <- ggplot(tidy_3, aes(x = date, y = value, color = key)) +
  geom_line() +
  scale_color_manual(values = c("#000000", "#5F5F5F")) +
  ggtitle("Mean of Sampled and Non-sampled Random Forest models") +
  myblanktheme

tidy_4 <- gather(graph_df, key = "key", value = "value", "naive", "infl")
plot_4 <- ggplot(tidy_4, aes(x = date, y = value, color = key)) +
  geom_line() +
  scale_color_manual(values = c("#000000", "#5F5F5F")) +
  ggtitle("Naive model") +
  myblanktheme

grid.arrange(plot_1, plot_2, plot_3, plot_4, nrow = 2, ncol = 2,
             top = "Inflation plotted versus models")

# Export -----------------------------------------
write_excel_csv(graph_df, paste0(export, "custom_forest_analysis/graph_df.csv"))
