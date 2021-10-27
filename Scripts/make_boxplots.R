# spearman's correlation across space and time or both


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


levels <- c('A', 'T', 'DOC_mgL',
            'NH4_ugL', 'NO3NO2_ugL', 'SRP_ugL',
            'TN_ugL', 'TP_ugL', 'Chla_ugL',
            'HIX', 'BIX', 'TN_TP', 'DP_TP', 'DN_TN')
labels <- c('Autoch', 'Alloch', 'DOC',
            'NH4', 'NO3', 'SRP',
            'TN', 'TP', 'Chl-a',
            'HIX', 'BIX', 'TN_TP', 'DP_TP', 'DN_TN')
names(labels) <- levels
long$variable <- factor(long$variable, levels = levels)



ggplot(data = long, aes(x = variable, y = value, fill = as.factor(Reservoir))) +
  facet_wrap(~variable, scales = 'free') +
  geom_boxplot()

ggplot(data = long, aes(x = as.factor(distance_from_stream), y = value)) +
  facet_wrap(~variable, scales = 'free') +
  geom_boxplot()

ggplot(data = long[long$distance_from_stream>0,], aes(x = as.factor(distance_from_stream), 
                                                      y = value, 
                                                      fill = as.factor(Reservoir))) +
  facet_wrap(~variable, scales = 'free') +
  geom_boxplot() +
  geom_point(position=position_jitterdodge(),alpha=0.3) #+
  theme(legend.position = 'none')
  #geom_jitter(width=0.1,alpha=0.2)  


ggplot(data = long[long$distance_from_stream>0,], aes(x = as.factor(month(Date)), y = value, fill = as.factor(Reservoir))) +
  facet_wrap(~variable, scales = 'free') +
  geom_boxplot() +
  geom_jitter(width=0.1,alpha=0.2) +
  theme(legend.position = 'none')

# make correlations for all
unique(long$variable)
for(i in 1:length(unique(long$variable))){
  unique(long$variable[i])
  x <- long[long$variable==unique(long$variable[i]),]

}
l <- rnorm(20, 2, 1)
m <- rnorm(20, 1, 2)
cor.test(l, m, use = 'complete.obs', method = 'spearman')
