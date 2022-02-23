# script to calculate range, mean, and median for all variables in RC dataset
library(tidyverse)

rc <- read.csv('./Data/continuum_data.csv')
vars <- c("TN_ugL", "TP_ugL", "NH4_ugL", "NO3NO2_ugL", "SRP_ugL", "DOC_mgL", "Chla_ugL", "A", "T")

rc_long <- rc %>% 
  select(Date, res_site, Reservoir, Site, vars) %>% 
  pivot_longer(vars, names_to = 'variable')

rc_long <- rc_long %>% 
  group_by(res_site, variable) %>% 
  mutate(min = min(value, na.rm = TRUE)) %>% 
  mutate(max = max(value, na.rm = TRUE)) %>% 
  mutate(mean = mean(value, na.rm = TRUE)) %>% 
  mutate(median = min(value, na.rm = TRUE)) 

table <- rc_long %>% 
  select(Reservoir, Site, variable, min, max, mean, median) %>% 
  distinct(Reservoir, Site, variable, .keep_all = TRUE) %>% 
  filter(res_site != 'FCR_102' & res_site != 'FCR_101' & res_site != 'FCR_100')

write.csv(table, './Data/summary_stats.csv', row.names = FALSE)
