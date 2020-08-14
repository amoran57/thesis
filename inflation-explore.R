# Header ---------------------------------------
rm(list=ls()) 
header <- source("header.R")

#Code ------------------------------------------
df <- read.csv(paste0(thesis, "CPIAUCSL.csv")) %>% 
  select(date = DATE, cpi = CPIAUCSL) %>% 
  mutate(date = as.Date(date),
         l_cpi = log(cpi),
         l_cpi_1 = lag(l_cpi),
         infl = l_cpi - l_cpi_1) %>% 
  select(date, infl)

model <- arma(df$infl[-1], order = c(2,2))
summary(model)

predictions <- df[-1,]
predictions$arma2 <- model$fitted.values
