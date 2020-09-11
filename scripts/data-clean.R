# Header ---------------------------------------
rm(list=ls()) 
header <- source("header.R")

#Import ------------------------------------------
infl <- read.csv(paste0(import, "CPIAUCSL.csv")) %>% 
  select(date = DATE, cpi = CPIAUCSL) %>% 
  mutate(date = as.Date(date),
         l_cpi = log(cpi),
         l_cpi_1 = dplyr::lag(l_cpi),
         infl = l_cpi - l_cpi_1) %>% 
  select(date, infl)

infl_na <- read.csv(paste0(import, "CPIAUCNS.csv")) %>% 
  select(date = DATE, cpi = CPIAUCNS) %>% 
  mutate(date = as.Date(date),
         l_cpi = log(cpi),
         l_cpi_1 = dplyr::lag(l_cpi),
         infl_na = l_cpi -l_cpi_1) %>% 
  select(date, infl_na)

rate10yr <- read.csv(paste0(import, "DGS10.csv")) %>% 
  select(date = DATE, rate10yr = DGS10) %>% 
  mutate(date = as.Date(date), rate10yr = base::as.numeric(rate10yr))

rate3month <- read.csv(paste0(import, "TB3MS.csv")) %>% 
  select(date = DATE, rate3month = TB3MS) %>% 
  mutate(date = as.Date(date)) 

rate12month <- read.csv(paste0(import, "TB1YR.csv")) %>% 
  select(date = DATE, rate12month = TB1YR) %>% 
  mutate(date = as.Date(date)) 

unemp <- read.csv(paste0(import, "UNRATE.csv")) %>% 
  select(date = DATE, unemp = UNRATE) %>% 
  mutate(date = as.Date(date))

unemp_na <- read.csv(paste0(import, "UNRATENSA.csv")) %>% 
  select(date = DATE, unemp_na = UNRATENSA) %>% 
  mutate(date = as.Date(date))

nat_unemp <- read.csv(paste0(import, "NROU.csv")) %>% 
  select(date = DATE, nat_unemp = NROU) %>% 
  mutate(date = as.Date(date))

nat_unemp_short <- read.csv(paste0(import, "NROUST.csv")) %>% 
  select(date = DATE, nat_unemp_short = NROUST) %>% 
  mutate(date = as.Date(date))

spread <- read.csv(paste0(import, "T10YIE.csv")) %>% 
  select(date = DATE, spread = T10YIE) %>% 
  mutate(date = as.Date(date))

survey <- read.csv(paste0(import, "MICH.csv")) %>% 
  select(date = DATE, survey = MICH) %>% 
  mutate(date = as.Date(date))

#Clean ------------------------------------------
dates <- as.data.frame(seq(as.Date("1949-01-01"), as.Date("2030-12-01"), by="month"))
names(dates) <- c("date")
nat_unemp_clean <- left_join(dates, nat_unemp) %>% 
  mutate(nat_unemp = ifelse(is.na(nat_unemp), dplyr::lag(nat_unemp), nat_unemp)) %>%
  mutate(nat_unemp = ifelse(is.na(nat_unemp), dplyr::lag(nat_unemp), nat_unemp))

nat_unemp_short_clean <- left_join(dates, nat_unemp_short) %>% 
  mutate(nat_unemp_short = ifelse(is.na(nat_unemp_short), dplyr::lag(nat_unemp_short), nat_unemp_short)) %>%
  mutate(nat_unemp_short = ifelse(is.na(nat_unemp_short), dplyr::lag(nat_unemp_short), nat_unemp_short))

#Merge ------------------------------------------
df <- infl %>% left_join(infl_na) %>% 
  left_join(rate10yr) %>% 
  left_join(rate3month) %>% 
  left_join(rate12month) %>% 
  left_join(unemp) %>% 
  left_join(unemp_na) %>% 
  left_join(nat_unemp_clean) %>% 
  left_join(nat_unemp_short_clean) %>% 
  left_join(spread) %>% 
  left_join(survey) %>% 
  mutate(year = lubridate::year(date))


#Export ------------------------------------------
write_rds(df, paste0(export,"master_data.rds"))
