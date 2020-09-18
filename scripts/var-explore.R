# Header ---------------------------------------
rm(list=ls()) 
header <- source("header.R")

#Code ------------------------------------------
df <- read_rds(paste0(export, "master_data.rds")) %>% 
  filter(year >= 1962) %>% 
  dplyr::select(infl, rate10yr, unemp, nat_unemp, year)

df_train <- df %>% 
  filter(year <= 2018)

df <- df %>% 
  dplyr::select(-year)

infl_ts <- ts(df$infl, start = c(1962, 1), frequency = 12)
df_ts <- ts(df, start = c(1962, 1), frequency = 12)
df_train_ts <- ts(df_train, start = c(1962, 1), frequency = 12)

#Set model ----------------------------------
model <- VAR(df_ts)

prediction <- predict(model, n.ahead = 12)
prediction <- prediction$fcst
prediction <- as.data.frame(prediction) %>% 
  dplyr::select(infl = infl.fcst)

pred_ts <- ts(prediction, start = c(2019, 1), frequency = 12)
accuracy(pred_ts, infl_ts)



