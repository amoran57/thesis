# Header ---------------------------------------
rm(list=ls()) 
header <- source("header.R")

#Import -------------------------------
zt <- read_rds(paste0(simulate_ar1, "evolving-data/simulated-evolving.rds"))
obj_fit <- read_rds(paste0(simulate_ar1, "evolving-data/obj-fit-evolving.rds"))
pred_fit <- read_rds(paste0(simulate_ar1, "evolving-data/pred-fit-evolving.rds"))
mean_fit <- read_rds(paste0(simulate_ar1, "evolving-data/mean-fit-evolving.rds"))
model <- auto.arima(zt)
arima_fit <- model$fitted

arima_ts <- ts(arima_fit[-c(1:11)])
obj_ts <- ts(obj_fit)
pred_ts <- ts(pred_fit)
mean_ts <- ts(mean_fit)
z_ts <- ts(zt[-c(1:11)])

#Compare --------------------------------------
graph_df <- data.frame(index = seq(1, length(obj_fit)), 
                       simulated = zt[-c(1:11)], 
                       arima = arima_fit[-c(1:11)], 
                       obj = obj_fit,
                       pred = pred_fit,
                       mean = mean_fit)
tidy_graph_df <- gather(data = graph_df, key = "key", value = "value", "simulated":"mean")
plot <- ggplot(data = tidy_graph_df, aes(x = index, y = value, color = key)) +
  geom_line()
plot

accuracy(z_ts, arima_ts)
accuracy(z_ts, obj_ts)
accuracy(z_ts, pred_ts)
accuracy(z_ts, mean_ts)

