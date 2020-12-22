# Header ---------------------------------------
rm(list=ls()) 
header <- source("header.R")

#Set up for tree -------------------------------
yt <- read_rds(paste0(simulate_ar1, "simulated-data.rds"))
obj_fit <- read_rds(paste0(simulate_ar1, "obj-fit.rds"))
pred_fit <- read_rds(paste0(simulate_ar1, "pred-fit.rds"))
model <- auto.arima(yt)
arima_fit <- model$fitted

arima_ts <- ts(arima_fit[-c(1:11)])
obj_ts <- ts(obj_fit)
pred_ts <- ts(pred_fit)
y_ts <- ts(yt[-c(1:11)])

#Compare --------------------------------------
graph_df <- data.frame(index = seq(1, length(obj_fit)), 
                       simulated = yt[-c(1:11)], 
                       arima = arima_fit[-c(1:11)], 
                       obj = obj_fit,
                       pred = pred_fit)
tidy_graph_df <- gather(data = graph_df, key = "key", value = "value", "simulated":"pred")
plot <- ggplot(data = tidy_graph_df, aes(x = index, y = value, color = key)) +
  geom_line()
plot

accuracy(y_ts, arima_ts)
accuracy(y_ts, obj_ts)
accuracy(y_ts, pred_ts)

