# Header ---------------------------------------
rm(list=ls()) 
header <- source("header.R")

#Code ------------------------------------------
forest <- read_rds(paste0(export, "multivariate/forecast.rds"))
base <- read_rds(paste0(export, "multivariate/base_forecast.rds"))
var <- read_rds(paste0(export, "multivariate/var_forecast.rds"))
df <- read_rds(paste0(export, "master_data.rds")) 
values_df <- df %>% 
  filter(year >= 1962)
tsData <- ts(values_df$infl, start = c(1962,1), frequency = 12)
naive <- window(tsData, start = c(1998,12), end = c(2019,12))
naive_forecast <- ts(naive, start = c(1999, 1), frequency = 12)

#Analysis
accuracy(tsData, forest)
accuracy(tsData, base)
accuracy(tsData, var)
accuracy(tsData, naive_forecast)
