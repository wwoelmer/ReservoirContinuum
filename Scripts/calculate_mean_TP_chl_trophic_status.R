# calculate historical range of chl-a secci and TP to estimate trophic status of FCR and BVR

# filtered chl-a
chl <- read.csv('./Data/raw_data/chl_all_edi.csv')
colnames(chl)[1] <- "Reservoir"
chl <- chl %>% 
  filter(Reservoir=='FCR' | Reservoir=='BVR' & Site == '50') %>% 
  filter(Depth_m == '0.1') %>% 
  select(Reservoir, Site, DateTime, Chla_ugL)
  
mean_chl <- chl %>% 
  group_by(Reservoir) %>% 
  mutate(min = min(Chla_ugL, na.rm = TRUE),
         max = max(Chla_ugL, na.rm = TRUE),
         mean = mean(Chla_ugL, na.rm = TRUE)) %>% 
  distinct(Reservoir, min, .keep_all = TRUE) %>% 
  select(-c(Site, DateTime, Chla_ugL))

mean_chl

# total phosphorus
p <- read.csv('./Data/raw_data/chem_all_edi.csv')
p <- p %>% 
  filter(Reservoir=='FCR' | Reservoir=='BVR' & Site == '50') %>% 
  filter(Depth_m == '0.1') %>% 
  select(Reservoir, Site, DateTime, TP_ugL)

mean_p <- p %>% 
  group_by(Reservoir) %>% 
  mutate(min = min(TP_ugL, na.rm = TRUE),
         max = max(TP_ugL, na.rm = TRUE),
         mean = mean(TP_ugL, na.rm = TRUE)) %>% 
  distinct(Reservoir, min, .keep_all = TRUE) %>% 
  select(-c(Site, DateTime, TP_ugL))

mean_p

