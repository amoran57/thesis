# Header ---------------------------------------
rm(list=ls()) 
header <- source("header.R")

#Import -------------------------------
yt <- read_rds(paste0(simulate_ar1, "ar1-data/simulated-data.rds"))
obj_fit <- read_rds(paste0(simulate_ar1, "ar1-data/obj-fit-ar.rds"))
pred_fit <- read_rds(paste0(simulate_ar1, "ar1-data/pred-fit-ar.rds"))
mean_fit <- read_rds(paste0(simulate_ar1, "ar1-data/mean-fit-ar.rds"))
model <- auto.arima(yt)
arima_fit <- model$fitted

arima_ts <- ts(arima_fit[-c(1:11)])
obj_ts <- ts(obj_fit)
pred_ts <- ts(pred_fit)
mean_ts <- ts(mean_fit)
y_ts <- ts(yt[-c(1:11)])
naive_ts <- ts(yt[-c(1:10, 501)])

#Compare --------------------------------------
graph_df <- data.frame(index = seq(1, length(obj_fit)), 
                       simulated = yt[-c(1:11)], 
                       naive = naive_ts,
                       arima = arima_fit[-c(1:11)], 
                       obj = obj_fit,
                       pred = pred_fit,
                       mean = mean_fit)
tidy_graph_df <- gather(data = graph_df, key = "key", value = "value", "simulated":"mean")
plot <- ggplot(data = tidy_graph_df, aes(x = index, y = value, color = key)) +
  geom_line()
plot

accuracy(y_ts, arima_ts)
accuracy(y_ts, obj_ts)
accuracy(y_ts, pred_ts)
accuracy(y_ts, mean_ts)
accuracy(y_ts, naive_ts)

# Again, with forecasts --------------------------------
#Import -------------------------------
yt <- read_rds(paste0(simulate_ar1, "ar1-data/simulated-data.rds"))
obj_forecast <- read_rds(paste0(simulate_ar1, "ar1-data/obj-fit-forecast.rds"))
pred_forecast <- read_rds(paste0(simulate_ar1, "ar1-data/pred-fit-forecast.rds"))
mean_forecast <- read_rds(paste0(simulate_ar1, "ar1-data/mean-fit-forecast.rds"))
arima_forecast <- read_rds(paste0(simulate_ar1, "ar1-data/arima-forecast.rds"))

arima_ts <- ts(arima_forecast)
obj_ts <- ts(obj_forecast)
pred_ts <- ts(pred_forecast)
mean_ts <- ts(mean_forecast)
naive_ts <- ts(yt[c(410:510)])
y_ts <- ts(yt[c(411:511)])

#Compare --------------------------------------
graph_df <- data.frame(index = seq(1, length(obj_forecast)), 
                       simulated = yt[c(411:511)], 
                       naive = naive_ts,
                       arima = arima_forecast, 
                       pred = pred_forecast,
                       mean = mean_forecast,
                       obj = obj_forecast)
tidy_graph_df <- gather(data = graph_df, key = "key", value = "value", "simulated":"obj")
plot <- ggplot(data = tidy_graph_df, aes(x = index, y = value, color = key)) +
  geom_line()
plot

accuracy(y_ts, arima_ts)
accuracy(y_ts, obj_ts)
accuracy(y_ts, pred_ts)
accuracy(y_ts, mean_ts)
accuracy(y_ts, naive_ts)

