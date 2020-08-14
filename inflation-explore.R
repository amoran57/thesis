# Header ---------------------------------------
rm(list=ls()) 
header <- source("header.R")

#Code ------------------------------------------
df <- read.csv(paste0(thesis, "CPIAUCSL.csv")) %>% 
  select(date = DATE, cpi = CPIAUCSL) %>% 
  mutate(date = as.Date(date),
         l_infl = log(cpi))
