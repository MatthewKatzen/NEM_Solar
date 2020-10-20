#Clean1 Get Solar Data

# Packages

library(tidyverse)
library(tidyr)
library(lubridate)
library(data.table)
library(janitor)
# library(readxl)
# library(writexl)
# library(xtable)
# library(nlme)
# library(scales)
# library(zoo)
Sys.setenv(TZ='UTC')

# GET DATA
###################################

generator_details_AEMO <- fread("D:/Data/RAW/AEMO/Website/generators_and_loads.csv") %>% clean_names() %>% 
  distinct(duid, .keep_all = TRUE) %>% 
  select(duid, station_name, region, fuel_source_primary, technology_type_descriptor, reg_cap_mw) %>% 
  filter(fuel_source_primary == "Solar") %>% 
  group_by(station_name) %>% #combine duplicate duids DAYDSF2 NUMURSF2 as done in NEMSIGHT, for some reason max(initialmw) is the sum of the two caps
  mutate(reg_cap_mw = sum(as.numeric(reg_cap_mw))) %>% 
  filter(duid == duid[1])

solar_data <- fread("D:/Data/Cleaned/INITIALMW/full_lmp_uncapped_initialmw.csv", 
                            select = c("settlementdate", "duid", "initialmw", "rrp", "lmp")) %>% 
  mutate(settlementdate = ymd_hms(settlementdate)) %>%
  left_join(generator_details_AEMO, by = "duid") %>%
  filter(fuel_source_primary == "Solar")
#note: this doesn't account for expansion of capacity!

# PLOTS
##################################################

#boxplot of daily mean_mw
solar_data %>%  
  mutate(time = ymd_hms(paste0("2019/01/01 ", substr(settlementdate,12,20))),
         date = ymd(substr(settlementdate, 1, 10)),
         month = as.factor(month(settlementdate))) %>% 
  group_by(date, duid, month) %>% 
  summarise(mean_mw = mean(initialmw)) %>% 
  #filter(mean_mw>10) %>% 
  ggplot(aes(y = mean_mw, fill = month, x = month))+
  geom_boxplot()+
  facet_wrap(~duid, scales = "free_y")+
  labs(title = "Solar Farm Mean MW")+
  ggsave("Output/Solar_MeanMW_Boxplots.png", width = 15, height = 10)

solar_data %>% 
  filter(duid %in% c("BNGSF1","MOREESF1", "NYNGAN1","PARSF1")) %>%  
  mutate(time = ymd_hms(paste0("2019/01/01 ", substr(settlementdate,12,20))),
         date = ymd(substr(settlementdate, 1, 10)),
         month = as.factor(month(settlementdate))) %>% 
  group_by(date, duid, month) %>% 
  summarise(mean_mw = mean(initialmw)) %>% 
  #filter(mean_mw>10) %>% 
  ggplot(aes(y = mean_mw, fill = month, x = month))+
  geom_boxplot()+
  facet_wrap(~duid, scales = "free_y")+
  labs(title = "Solar Farm Mean MW")+
  ggsave("Output/Solar_MeanMW_Boxplots_4.png", width = 10, height = 7)

top_solar_gens <- generator_details_AEMO %>% filter(duid %in% c("BNGSF1","MOREESF1", "NYNGAN1","PARSF1")) %>% 
  arrange(duid) %>% 
  cbind(weather_station = c("Port Augusta Aero",
                            "Moree Aero",
                            "Girilambone (Okeh) AWS",
                            "Condobolin Airport AWS")) %>% 
  data.frame()



# which gens are constrained the most
temp <- solar_data %>% mutate(constrained = (lmp!=rrp)) %>% 
  group_by(duid, month = month(settlementdate)) %>% 
  summarise(perc_constrained = sum(constrained)/n())

temp %>% filter(duid %in% top_solar_gens$duid) %>% 
  ggplot(aes(x = month, y = perc_constrained, colour = duid))+
  geom_line()

temp %>% mutate(top = (duid %in% top_solar_gens$duid)) %>% 
  ggplot(aes(x = month, y = perc_constrained, group = duid, size = top, alpha = top))+
  geom_line()+
  scale_alpha_discrete(range = c(0.1, 1))+
  scale_size_discrete(range = c(1, 3))



#Plots 
#####################
solar_duid <- top_solar_gens[3,1]
weather_station <- top_solar_gens[3,7]

#Initialmw v time, grouped by month
solar_data %>% filter(duid == solar_duid) %>% 
  mutate(time = ymd_hms(paste0("2019/01/01 ", substr(settlementdate,12,20))),
         date = ymd(substr(settlementdate, 1, 10)),
         month = month(settlementdate)) %>% 
  group_by(date) %>% 
  #filter(!any(initialmw==0 & (time %within% interval(ymd_hms("2019/01/01 10:00:00 UTC"), 
  #                                               ymd_hms("2019/01/01 15:00:00 UTC"))))) %>% #remove day if constrained
  ggplot(aes(x = time, y = initialmw, group = date)) +
  geom_line() + 
  facet_wrap(~month)

solar_data %>% filter(duid == solar_duid) %>% 
  mutate(time = ymd_hms(paste0("2019/01/01 ", substr(settlementdate,12,20))),
         date = ymd(substr(settlementdate, 1, 10)),
         month = month(settlementdate)) %>% 
  group_by(month, time) %>% 
  summarise(mean_mw = mean(initialmw)) %>% 
  ggplot(aes(x = time, y = mean_mw, group = month)) +
  geom_line() + 
  facet_wrap(~month)


# Reg
#############################

reg_data <- solar_data %>% filter(duid == solar_duid) %>% 
  mutate(time = ymd_hms(paste0("2019/01/01 ", substr(settlementdate,12,20))),
         date = ymd(substr(settlementdate, 1, 10))) %>% 
  group_by(date) %>% 
  #filter(!any(initialmw==0 & (time %within% interval(ymd_hms("2019/01/01 10:00:00 UTC"), 
  #                                                   ymd_hms("2019/01/01 15:00:00 UTC"))))) %>% #remove constrained days
  group_by(date) %>% 
  summarise(mean_mw = mean(initialmw)) %>% 
  mutate(month = as.factor(month(date))) %>% #add max temp
  left_join(list.files(paste0("D:/Data/RAW/BOM/",weather_station,"/"),full.names = TRUE)[1] %>% fread() %>% clean_names() %>% 
              mutate(date = ymd(paste(year, month, day, sep = "/")),
                     max_temp = maximum_temperature_degree_c) %>% 
              select(date, max_temp),
            by = "date") %>% #add solar exposure
  left_join(list.files(paste0("D:/Data/RAW/BOM/",weather_station,"/"),full.names = TRUE)[2] %>% fread() %>% clean_names() %>% 
              mutate(date = ymd(paste(year, month, day, sep = "/")),
                     solar_exp = daily_global_solar_exposure_mj_m_m) %>% 
              select(date, solar_exp),
            by = "date")


# Plots
######################
reg_data %>% 
  ggplot(aes(x = max_temp, y = mean_mw))+
  geom_point()

reg_data %>% 
  ggplot(aes(x = solar_exp, y = mean_mw))+
  geom_point()

reg_data %>% 
  ggplot(aes(x = solar_exp, y = max_temp))+
  geom_point()

reg_data %>% 
  #mutate(exp_mean = frollmean(solar_exp, n=5, align = "center")) %>% 
  pivot_longer(cols = c(mean_mw, max_temp, solar_exp)) %>% 
  ggplot(aes(x = date, y = value))+
  geom_line() + 
  facet_wrap(~name, ncol=1)

reg_data %>% select(-mean_mw, -max_temp) %>% 
  mutate(exp_mean = rollapply(data = solar_exp,
                              width = 5, 
                              FUN = function(x) median(x[-3], na.rm = TRUE), 
                              align = "center", 
                              fill = NA)) %>% 
  pivot_longer(cols = c(solar_exp, exp_mean)) %>% 
  ggplot(aes(x = date, y = value, colour = name))+
  geom_line()

#regression
#################

temp <- reg_data %>% mutate(quarter = as.factor(lubridate::quarter(date, fiscal_start = 12))) %>%
  filter(mean_mw!=0) %>%
  mutate(overcast = (solar_exp < frollmean(solar_exp, align = "center", n=5))) %>% 
  lm((mean_mw) ~ max_temp + I(max_temp^2) + overcast, data = .)


temp <- reg_data %>% mutate(quarter = as.factor(lubridate::quarter(date, fiscal_start = 12))) %>% 
  filter(mean_mw!=0) %>% 
  lm((mean_mw) ~ max_temp + I(max_temp^2) + quarter, data = .)

temp %>% summary

plot(temp, which = 3)

qplot(temp$model$solar_exp, temp$residuals)
qplot(temp$model$max_temp, temp$residuals)
qplot(temp$model$mean_mw)
temp$model$overcast
