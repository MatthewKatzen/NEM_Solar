#Clean1 Get Solar Data

# Packages

library(tidyverse)
library(tidyr)
library(lubridate)
library(data.table)
library(janitor)
library(readxl)
library(writexl)
library(xtable)
library(nlme)
library(scales)
library(zoo)
Sys.setenv(TZ='UTC')

# GET DATA
###################################

generator_details_AEMO <- fread("D:/Data/RAW/AEMO/Website/generators_and_loads.csv") %>% clean_names() %>% 
  distinct(duid, .keep_all = TRUE) %>% select(duid, station_name, region, fuel_source_primary, technology_type_descriptor)


solar_data <- fread("D:/Data/Cleaned/INITIALMW/full_lmp_uncapped_initialmw.csv", 
                            select = c("settlementdate", "duid", "initialmw", "rrp", "lmp")) %>% 
  mutate(settlementdate = ymd_hms(settlementdate)) %>% 
  left_join(generator_details_AEMO, by = "duid") %>% 
  filter(fuel_source_primary == "Solar")


temp <- solar_data %>% mutate(constrained = (lmp!=rrp)) %>% 
  group_by(station_name) %>% 
  summarise(perc_constrained = sum(constrained)/n()*100, total_output = sum(initialmw)/12)

#Beryl Solar Farm (BERYLSF1)
# Constrained 0.017% 131GWh 2019

beryl_data <- solar_data %>% filter(duid == "BERYLSF1")

bom_max <- list.files("D:/Data/RAW/BOM/",full.names = TRUE)[1] %>% fread() %>% clean_names() %>% 
  mutate(date = ymd(paste(year, month, day, sep = "/")),
         max_temp = maximum_temperature_degree_c) %>% 
  select(date, max_temp)

bom_solar <- list.files("D:/Data/RAW/BOM/",full.names = TRUE)[3] %>% fread() %>% clean_names() %>% 
  mutate(date = ymd(paste(year, month, day, sep = "/")),
         solar_exp = daily_global_solar_exposure_mj_m_m) %>% 
  select(date, solar_exp)

bom_data <- bom_max %>% left_join(bom_solar, by = "date")

# BERYL PLOTS
#################################
beryl_data %>% 
  mutate(time = ymd_hms(paste0("2019/01/01 ", substr(settlementdate,12,20))),
         date = ymd(substr(settlementdate, 1, 10)),
         month = month(settlementdate)) %>% 
  ggplot(aes(x = time, y = initialmw, group = date)) +
  geom_line() + 
  facet_wrap(~month)+
  ggsave("Output/Beryl_monthly_mean_mw.png")


beryl_data %>% 
  mutate(time = ymd_hms(paste0("2019/01/01 ", substr(settlementdate,12,20))),
         date = ymd(substr(settlementdate, 1, 10))) %>% 
  filter(date %in% beryl_stats$date[c(2,5)]) %>% 
  mutate(date = as.factor(date)) %>% 
  ggplot(aes(x = time, y = initialmw, colour = date)) +
  geom_line()

# Regression 
#############################

#mean of day
beryl_stats <- beryl_data %>% 
  mutate(time = ymd_hms(paste0("2019/01/01 ", substr(settlementdate,12,20))),
         date = ymd(substr(settlementdate, 1, 10))) %>% 
  group_by(date) %>% 
  summarise(mean_mw = mean(initialmw)) %>% 
  left_join(bom_data, by = "date") %>% 
  mutate(max_temp_2 = max_temp^2,
         month = as.factor(month(date)))

reg <- beryl_stats[-c(1:29),] %>% 
  lm(mean_mw ~ max_temp + max_temp_2 + solar_exp + month, data = .)

summary(reg)

reg <- beryl_stats %>% filter(month(date)>5) %>% 
  lm(mean_mw ~ max_temp + max_temp_2 + solar_exp + month, data = .)

summary(reg)
# hourly
beryl_stats_hourly <- beryl_data %>% 
  mutate(time = ymd_hms(paste0("2019/01/01 ", substr(settlementdate,12,20))),
         date = ymd(substr(settlementdate, 1, 10)),
         hour = hour(settlementdate),
         hour = ifelse(hour %in% c(0:4,19:23),
                       "night",
                       hour)) %>%  #group night hours into one group
  group_by(date, hour) %>% 
  summarise(mean_mw = mean(initialmw)) %>% 
  left_join(bom_data, by = "date") %>% 
  mutate(max_temp_2 = max_temp^2,
         month = as.factor(month(date)),
         hour = as.factor(hour))


reg <- lm(mean_mw ~ max_temp + max_temp_2 + solar_exp + month + hour, data = beryl_stats_hourly)

summary(reg)
