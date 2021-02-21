# Header ---------------------------------------
rm(list=ls())
header <- source("header.R")
library(car)

#Code ------------------------------------------
us_df <- read_rds(paste0(export, "master_data.rds"))
uk_df <- read.csv(paste0(import, "UKCPI.csv"))

#Test US unemployment values for stationarity ----------------
us_values <- us_df %>% 
  select(date, unemp, rate3month)

unemp_ts <- ts(us_values$unemp[-c(1:12)], start = c(1948, 1), frequency = 12)

plot(unemp_ts)
#test residuals
adf.test(unemp_ts)
dickey_fuller <- ur.df(unemp_ts, type = "none", lags = 0)
summary(dickey_fuller)

#It is not stationary. Let's try to detrend it:
unemp_df <- us_values[-c(1:12),] %>% select(date, unemp)
rownames(unemp_df) <- seq(1, nrow(unemp_df))
unemp_df$trend <- seq(1, nrow(unemp_df))
unemp_df$month <- c(rep(seq(1,12), 72), 1,2,3,4,5,6,7,8)
unemp_df$month <- as.factor(unemp_df$month)
unemp_df$tf <- c(TRUE)
unemp_df <- pivot_wider(unemp_df, names_from = month, values_from = tf, values_fill = c(FALSE))
unemp_df <- unemp_df[,-4]
names(unemp_df) <- c("date", "unemp", "trend", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

model <- glm(unemp ~ trend + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec, data = unemp_df)
summary(model)
#It seems like only the trend is important
trend_model <- glm(unemp ~ trend, data = unemp_df)
summary(trend_model)

detrend_unemp <- trend_model$residuals
detrend_unemp_ts <- ts(detrend_unemp, start = c(1948, 1), frequency = 12)
plot(detrend_unemp_ts)
plot(unemp_ts)

adf.test(detrend_unemp_ts)
dickey_fuller <- ur.df(detrend_unemp_ts, type = "none", lags = 0)
summary(dickey_fuller)

#This is not much better. Let's take the difference
diff_unemp_ts <- diff(detrend_unemp_ts)
plot(diff_unemp_ts)

adf.test(diff_unemp_ts)
dickey_fuller <- ur.df(diff_unemp_ts, type = "none", lags = 0)
summary(dickey_fuller)

dwmodel <- glm(diff_unemp_ts ~ stats::lag(diff_unemp_ts))
durbinWatsonTest(dwmodel)

#Write back to the dataframe
diff_df <- data.frame(diff_unemp_ts)
diff_df$date <- seq(as.Date("1948/2/1"), as.Date("2020/8/1"), "month")

new_us_values <- left_join(us_values, diff_df, by = "date")


#Test US 3-month rate values for stationarity -------------------
rate_ts <- ts(us_values$rate3month, start = c(1947, 1), frequency = 12)

plot(rate_ts)
#test residuals
adf.test(rate_ts)
dickey_fuller <- ur.df(rate_ts, type = "none", lags = 0)
summary(dickey_fuller)

#It is not stationary. Let's try to detrend it:
rate_df <- us_values %>% select(date, rate = rate3month)
rownames(rate_df) <- seq(1, nrow(rate_df))
rate_df$trend <- seq(1, nrow(rate_df))
rate_df$month <- c(rep(seq(1,12), 73), 1,2,3,4,5,6,7,8)
rate_df$month <- as.factor(rate_df$month)
rate_df$tf <- c(TRUE)
rate_df <- pivot_wider(rate_df, names_from = month, values_from = tf, values_fill = c(FALSE))
rate_df <- rate_df[,-4]
names(rate_df) <- c("date", "rate", "trend", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

rate_model <- glm(rate ~ trend + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec, data = rate_df)
summary(rate_model)

#Again the trend is more important
rate_trend_model <- glm(rate ~ trend, data = rate_df)
summary(rate_trend_model)

detrend_rate <- rate_trend_model$residuals
detrend_rate_ts <- ts(detrend_rate, start = c(1947,1), frequency = 12)
plot(detrend_rate_ts)
plot(rate_ts)

adf.test(detrend_rate_ts)
dickey_fuller <- ur.df(detrend_rate_ts, type = "none", lags = 0)
summary(dickey_fuller)

#Still not great, let's difference
diff_rate_ts <- diff(detrend_rate_ts)
plot(diff_rate_ts)

adf.test(diff_rate_ts)
dickey_fuller <- ur.df(diff_rate_ts, type = "none", lags = 0)
summary(dickey_fuller)

rate_dwmodel <- glm(diff_rate_ts ~ stats::lag(diff_rate_ts))
durbinWatsonTest(rate_dwmodel)

#Fair amount of autocorrelation, but what can you do
rate_df <- data.frame(diff_rate_ts)
rate_df$date <- seq(as.Date("1947/2/1"), as.Date("2020/8/1"), "month")

new_us_values <- left_join(new_us_values, rate_df, by = "date")

#Clean UK CPI to inflation ----------------------
uk_values <- uk_df[-c(1:172),]
colnames(uk_values) <- c("date", "cpi")
uk_values$date <- seq(as.Date("1988/1/1"), as.Date("2021/1/1"), "month")
uk_values$cpi <- as.character(uk_values$cpi)
uk_values$cpi <- as.numeric(uk_values$cpi)
uk_values <- uk_values %>% 
  mutate(l_cpi = log(cpi),
         l_cpi_1 = dplyr::lag(l_cpi),
         infl = l_cpi - l_cpi_1) %>% 
  dplyr::select(date, infl)
uk_values <- uk_values[-1,]
rownames(uk_values) <- seq(1, nrow(uk_values))

infl <- ts(uk_values$infl, start = c(1988, 2), frequency = 12)
plot(infl)
#test residuals
adf.test(infl)
dickey_fuller <- ur.df(infl, type = "none", lags = 0)
summary(dickey_fuller)

infl_dwmodel <- glm(infl ~ stats::lag(infl))
durbinWatsonTest(infl_dwmodel)

#Merge and export ---------------------------
values_df <- left_join(new_us_values, uk_values, by = "date") %>% 
  select(date, unemp = diff_unemp_ts, rate3month = diff_rate_ts, infl)

write_rds(values_df, paste0(export, "other_cases/built_data.rds"))
