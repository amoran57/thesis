# Header ---------------------------------------
rm(list=ls()) 
header <- source("header.R")

#Code ------------------------------------------
#import data
df <- read.csv(paste0(thesis, "CPIAUCSL.csv")) %>% 
  select(date = DATE, cpi = CPIAUCSL) %>% 
  mutate(date = as.Date(date),
         l_cpi = log(cpi),
         l_cpi_1 = dplyr::lag(l_cpi),
         infl = l_cpi - l_cpi_1) %>% 
  select(date, infl)

month_3 <- read.csv(paste0(thesis, "TB3MS.csv")) %>% 
  select(DATE, rate_3 = TB3MS) %>% 
  mutate(date = as.Date(DATE)) %>% 
  select(date, rate_3)

month_12 <- read.csv(paste0(thesis, "TB1YR.csv")) %>% 
  select(DATE, rate_12 = TB1YR) %>% 
  mutate(date = as.Date(DATE)) %>% 
  select(date, rate_12)

df <- left_join(df, month_3, by = "date")
df <- left_join(df, month_12, by = "date")

tsData <- ts(df$infl[-1], start = c(1947,2), frequency = 12)


groups <- createTimeSlices(tsData, 80, horizon = 5)
folds <- groupKFold(groups)

control <- trainControl(method = "timeslice", index = folds)
model <- train(infl ~ rate_3 + rate_12, data = df[-1,], trControl = control)
