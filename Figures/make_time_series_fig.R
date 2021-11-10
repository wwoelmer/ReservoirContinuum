# make time series figure

library(tidyverse)
library(patchwork)
library(lubridate)
library(EnvStats)
library(ggpubr)
library(ggpmisc)

r_col <- c('olivedrab3', 'royalblue1')


#data <- read.csv('./Data/continuum_pf.csv')
data <-read.csv('./Data/continuum_ww.csv')
data$Date <- as.Date(data$DateTime)
data$connectivity <- as.factor(data$connectivity)

data <- data %>% select(Date, everything(),-DateTime, -Depth_m, -(DIC_mgL:Flag_DN))
data <- data %>% mutate(TN_TP = TN_ugL/TP_ugL) %>% 
  mutate(DP_TP = SRP_ugL/TP_ugL) %>% 
  mutate(DN_TN = (NH4_ugL + NO3NO2_ugL)/TN_ugL)


data$distance_from_stream <- 'NA'
data <- data %>% unite('res_site', Reservoir:Site, remove = FALSE)
stream_sites <- c('FCR_100', 'FCR_200', 'FCR_102', 'FCR_101', 'FCR_99', 'BVR_100', 'BVR_200', 'FCR_1')
riv_sites <- c('FCR_20', 'BVR_20', 'BVR_30') 
trans_sites <- c('BVR_1', 'BVR_45', 'FCR_30', 'FCR_45')
lac_sites <- c('FCR_50', 'BVR_50')

for (i in 1:nrow(data)) {
  if(data$res_site[i] %in% stream_sites){
    data$distance_from_stream[i] <- '0'
  }else if(data$res_site[i] %in% riv_sites){
    data$distance_from_stream[i] <- '1'
  }else if(data$res_site[i] %in% trans_sites){
    data$distance_from_stream[i] <- '2'
  }else if(data$res_site[i] %in% lac_sites){
    data$distance_from_stream[i] <- '3'
  }
}

data$distance_from_stream <- as.numeric(data$distance_from_stream)
in_reservoir <- c( '20', '30', '45', '50') # site 01 is in res for BVR and in stream for FCR 
in_stream <- c('99', '100', '101', '102', '200')

### read in distance data from arcgis
dist <- read.csv('./Data/rc_coordinates_100only.csv')
dist <- dist %>% select(Reservoir, Site, distance_m)

data <- left_join(data, dist)

long <- data %>%   
  select(Site, Reservoir, Date, distance_from_stream, TN_ugL:Chla_ugL, A:BIX, TN_TP:distance_m) %>% 
  pivot_longer(TN_ugL:DN_TN, names_to = 'variable', values_to = 'value')


levels <- c('T', 'BIX', 'A', 'HIX',
            'DOC_mgL',  'Chla_ugL','TN_ugL', 'TP_ugL',
            'NH4_ugL', 'NO3NO2_ugL', 'SRP_ugL',
            'TN_TP', 'DP_TP', 'DN_TN')
labels <- c('T-autoch', 'BIX-autoch', 'A-alloch','HIX-alloch', 
            'DOC','Chl-a', 'TN', 'TP',
            'NH4', 'NO3', 'SRP',
            'TN_TP', 'DP_TP', 'DN_TN')
names(labels) <- levels
long$variable <- factor(long$variable, levels = levels)
long$Month <- as.factor(month(long$Date))

######################################################################################################################
### time series color by date, one for each reservoir

## FCR
ggplot(data = long[long$Reservoir=='FCR' & long$distance_from_stream > 0,], aes(x = distance_m, y = value, col = Month)) +
  geom_point() +
  geom_smooth(aes(col = Month), alpha = 0.5) +
  xlab('Distance from stream') +
  ylab('Concentration') +
  facet_wrap(~variable, scales = "free", labeller = labeller(variable = labels)) + 
  scale_color_manual(values = rev(hcl.colors(7, "RdYlGn"))) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'grey95'))

## BVR
ggplot(data = long[long$Reservoir=='BVR' & long$distance_from_stream > 0,], aes(x = distance_m, y = value, col = Month)) +
  geom_point() +
  geom_smooth(aes(col = Month)) +
  xlab('Distance from stream') +
  ylab('Concentration') +
  facet_wrap(~variable, scales = "free", labeller = labeller(variable = labels)) + 
  scale_color_manual(values = rev(hcl.colors(7, "RdYlGn"))) +
  theme_bw() +
  labs(col = 'Month') +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'grey95'))



######################################################################################################################
### group all data over distance from stream

# without stream data
ggplot(data = long[long$distance_from_stream > 0,], aes(x = distance_m, y = value)) +
  geom_point(aes(col = as.factor(Reservoir))) +
  geom_smooth() +
  xlab('Distance from stream') +
  ylab('Concentration') +
  facet_wrap(~variable, scales = "free", labeller = labeller(variable = labels)) + 
  scale_color_manual(values = r_col) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        #   legend.position = "none"
  )



# and with stream measurements
weir_stream <- c('102', '101', '100', '01')
ggplot(data = long[!(long$Site %in% weir_stream),], aes(x = distance_m, y = value)) +
  geom_point(aes(col = as.factor(Reservoir))) +
  geom_smooth() +
  xlab('Distance from stream') +
  ylab('Concentration') +
  facet_wrap(~variable, scales = "free", labeller = labeller(variable = labels)) + 
  scale_color_manual(values = r_col) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        #   legend.position = "none"
  )
