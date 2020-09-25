# Header ---------------------------------------
rm(list=ls()) 
header <- source("header.R")

#Code ------------------------------------------
df <- read_rds(paste0(export, "master_data.rds")) 
arima <- read_rds(paste0(export,"arima_expanding_horizon.rds"))
forest <- read_rds(paste0(export,"forest_expanding_horizon.rds"))
var <- read_rds(paste0(export,"var_expanding_horizon.rds"))

values_df <- df %>% 
  filter(year >= 1959)
tsData <- ts(values_df$infl, start = c(1959, 1), frequency = 12)


naive_forecast <- window(tsData, start = c(1999, 1), end = c(2019, 1))
naive_df <- as.data.frame(naive_forecast) %>% 
  dplyr::select(naive = x) %>% 
  mutate(date = seq(as.Date("2000/1/1"), as.Date("2020/1/1"), "month"),
         naive = base::as.numeric(naive))

infl_df <- as.data.frame(tsData) %>% 
  dplyr::select(infl = x) %>% 
  mutate(date = seq(as.Date("1959/1/1"), as.Date("2020/8/1"), "month"))

forecast_df <- infl_df %>% left_join(arima) %>% left_join(forest) %>% left_join(var) %>% left_join(naive_df) %>% 
  dplyr::select(date, everything())

forecast_month3 <- forecast_df %>% 
  dplyr::select(date, infl, ends_with("month3"), naive)

forecast_month6 <- forecast_df %>% 
  dplyr::select(date, infl, ends_with("month6"), naive)

forecast_month12 <- forecast_df %>% 
  dplyr::select(date, infl, ends_with("month12"), naive)
  

# Get accuracy -----------------------------------------
values <- names(forecast_df)
values <- values[-c(1:2)]
all_ts <- c("ME", "RMSE", "MAE", "MPE", "MAPE", "ACF1", "Theil's U")
all_ts <- as.data.frame(all_ts)
for (prediction in values) {
  temp_df <- forecast_df %>% 
    dplyr::select(date, temp = prediction) %>% 
    dplyr::filter(!is.na(temp))
  
  dates <- temp_df$date
  preds <- temp_df$temp
  start <- dates[1]
  start <- as.character(start)
  start_year <- substr(start, 1, 4)
  start_month <- substr(start, 6, 7)
  start_year <- as.numeric(start_year)
  start_month <- as.numeric(start_month)
  temp_ts <- ts(preds, start = c(start_year, start_month), frequency = 12)
  
  temp_accuracy <- accuracy(temp_ts, tsData)
  temp_accuracy <- as.data.frame(temp_accuracy)
  temp_vector <- c()
  accurate_names <- names(temp_accuracy)
  for (name in accurate_names) {
    temp_val <- as.numeric(temp_accuracy[name])
    temp_vector <- c(temp_vector, temp_val)
  }
  temp_accuracy_df <- data.frame(accurate_names, temp_vector) 
  names(temp_accuracy_df) <- c("all_ts", prediction)
  
  all_ts <- left_join(all_ts, temp_accuracy_df)
}


# plot results -----------------------------------
tidy_forecast_month3 <- gather(data = forecast_month3, key = "key", value = "value", "infl":"naive") %>% 
  mutate(year = lubridate::year(date)) %>% 
  filter(year > 1999 & year < 2020)

tidy_forecast_month6 <- gather(data = forecast_month6, key = "key", value = "value", "infl":"naive") %>% 
  mutate(year = lubridate::year(date)) %>% 
  filter(year > 1999 & year < 2020)

tidy_forecast_month12 <- gather(data = forecast_month12, key = "key", value = "value", "infl":"naive") %>% 
  mutate(year = lubridate::year(date)) %>% 
  filter(year > 1999 & year < 2020)

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

plot_month3 <- ggplot(data = tidy_forecast_month3, aes(x = date, y = value, color = key)) +
  geom_line() +
  scale_color_manual(values = c("purple4", "darkgreen", "black", "red", "gray")) +
  theme_minimal() +
  labs(
    title = "Forecasted monthly inflation",
    subtitle = "Predicted for 2000-2019 given 1959-2018 data",
    x = "Date",
    y = "Inflation"
  )

plot_month6 <- ggplot(data = tidy_forecast_month6, aes(x = date, y = value, color = key)) +
  geom_line() +
  scale_color_manual(values = c("purple4", "darkgreen", "black", "red", "gray")) +
  theme_minimal() +
  labs(
    title = "Forecasted monthly inflation",
    subtitle = "Predicted for 2000-2019 given 1959-2018 data",
    x = "Date",
    y = "Inflation"
  )

plot_month12 <- ggplot(data = tidy_forecast_month12, aes(x = date, y = value, color = key)) +
  geom_line() +
  scale_color_manual(values = c("purple4", "darkgreen", "black", "red", "gray")) +
  theme_minimal() +
  labs(
    title = "Forecasted monthly inflation",
    subtitle = "Predicted for 2000-2019 given 1959-2018 data",
    x = "Date",
    y = "Inflation"
  )

plot_month3
plot_month6
plot_month12

#export ------------------------------------------
write_rds(forecast_df, paste0(export,"forecast_expanding_horizon.rds"))
write_rds(all_ts, paste0(export, "method_accuracy_metrics.rds"))
