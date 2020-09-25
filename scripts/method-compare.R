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


naive_forecast <- window(tsData, start = c(1999, 1), end = c(2018, 12))
naive_df <- as.data.frame(naive_forecast) %>% 
  dplyr::select(naive = x) %>% 
  mutate(date = seq(as.Date("2000/1/1"), as.Date("2019/12/1"), "month"),
         naive = base::as.numeric(naive))

infl_df <- as.data.frame(tsData) %>% 
  dplyr::select(infl = x) %>% 
  mutate(date = seq(as.Date("1959/1/1"), as.Date("2020/8/1"), "month"))

forecast_df <- infl_df %>% left_join(arima) %>% left_join(forest) %>% left_join(var) %>% left_join(naive_df) %>% 
  dplyr::select(date, everything())


# plot results -----------------------------------
tidy_forecast <- gather(data = forecast_df, key = "key", value = "value", "infl":"naive") %>% 
  mutate(year = lubridate::year(date)) %>% 
  filter(year > 1999 & year < 2020)

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

plot_all <- ggplot(data = tidy_forecast, aes(x = date, y = value, color = key)) +
  geom_line() +
  scale_color_manual(values = c("purple4", "darkgreen", "black", "red", "gray")) +
  theme_minimal() +
  labs(
    title = "Forecasted monthly inflation",
    subtitle = "Predicted for 2000-2019 given 1959-2018 data",
    x = "Date",
    y = "Inflation"
  )

plot_all

#export ------------------------------------------
write_rds(forecast_df, paste0(export,"forecast_expanding_horizon.rds"))
