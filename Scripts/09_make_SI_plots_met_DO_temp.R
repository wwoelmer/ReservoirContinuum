# make plot of air temperature and precipitation over the study period
library(tidyverse)
library(ggpubr)
library(lubridate)


met <- read.csv('./Data/raw_data/met_edi.csv')
met$DateTime <- as.POSIXct(met$DateTime)
met <- met %>% 
  filter(met$DateTime < as.POSIXct('2019-10-05 00:00:00') & met$DateTime > as.POSIXct('2019-04-28 00:00:00'))

met_daily <- met %>% 
  select(DateTime, Rain_Total_mm, AirTemp_Average_C) %>% 
  group_by(DateTime) %>% 
  mutate(airtemp_avg = mean(AirTemp_Average_C, na.rm = TRUE)) %>% 
  mutate(rain_sum = sum(Rain_Total_mm, na.rm = TRUE)) %>% 
  distinct(DateTime, .keep_all = TRUE) %>% 
  select(DateTime, airtemp_avg, rain_sum)


dates <- c(as.Date('2019-04-29'), as.Date('2019-05-30'), as.Date('2019-06-27'), as.Date('2019-07-18'), 
           as.Date('2019-08-22'), as.Date('2019-09-20'), as.Date('2019-10-04'))


a <- ggplot(data = met_daily, aes(x = as.Date(DateTime), y = airtemp_avg)) +
  geom_line(size = 0.8) +
  geom_vline(xintercept = as.numeric(dates), color = 'dodgerblue2', size = 0.8) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text = element_text(size = 15)) +  
  xlab('Date') +
  ylab('Air Temperature (C)')


b <- ggplot(data = met_daily, aes(x = as.Date(DateTime), y = rain_sum)) +
  geom_line(size = 0.8) +
  geom_vline(xintercept = as.numeric(dates), color = 'dodgerblue2', size = 0.8) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text = element_text(size = 15)) +  
  xlab('Date') +
  ylab('Total Daily Rain (mm)')

ggarrange(a, b, nrow = 2)


# read in surface DO from each site
do <- read.csv('./Data/raw_data/spcond_edi.csv')
do$Date <- as.Date(do$DateTime)
do <- do %>% 
  filter(Reservoir=='BVR' | Reservoir=='FCR') %>%
  filter(Depth_m==0.1) %>% 
  select(Reservoir, Site, Date, Depth_m, DOSat, Temp_C) %>% 
  filter(Date %in% dates)


# there are duplicate entries with NA for some days/sites
do <- na.omit(do)
do$Site <- as.factor(do$Site)

# add distance
# read in distance data from arcgis
dist <- read.csv('./Data/raw_data/rc_coordinates_100only.csv')
dist <- dist %>% select(Reservoir, Site, distance_m)
dist$Site <- as.factor(dist$Site)


# join the two
do_dist <- left_join(do, dist)
do_dist <- do_dist %>% 
  pivot_longer(cols = c(DOSat, Temp_C), names_to = 'variable', values_to = "value") %>% 
  select(-Depth_m)
do_dist <- do_dist %>% 
  mutate(res_site = paste0(Reservoir, '_', Site)) %>% 
  filter(res_site != 'FCR_100' & res_site != 'FCR_101' & res_site != 'FCR_102' & res_site != 'FCR_1')

# plot DO and temp over space
b <- ggplot(data = do_dist[do_dist$Reservoir=='BVR',], aes(x = distance_m, y = value, col = as.factor(month(Date)))) +
  geom_point(size = 3) +
  geom_line(aes(col = as.factor(month(Date)))) +
  xlab('Distance from stream') +
  ylab('DO (% Saturation) or Temp (°C)') +
  facet_wrap(~variable, scales = "free") + 
  labs(color = 'Month') +
  scale_color_manual(values = rev(hcl.colors(7, "Zissou 1")), name = 'Month') +
  ggtitle('Beaverdam Reservoir') +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        # legend.position = "none",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        strip.text = element_text(size = 12))
b

f <- ggplot(data = do_dist[do_dist$Reservoir=='FCR',], aes(x = distance_m, y = value, col = as.factor(month(Date)))) +
  geom_point(size = 3) +
  geom_line(aes(col = as.factor(month(Date)))) +
  xlab('Distance from stream') +
  ylab('DO (% Saturation) or Temp (°C)') +
  facet_wrap(~variable, scales = "free") + 
  labs(color = 'Month') +
  scale_color_manual(values = rev(hcl.colors(7, "Zissou 1")), name = 'Month') +
  ggtitle('Falling Creek Reservoir') +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        # legend.position = "none",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        strip.text = element_text(size = 12))
f
siplot <- ggarrange(b, f, nrow = 2)
ggsave('./Figures/SIFig_watertemp_DO.png', siplot)
