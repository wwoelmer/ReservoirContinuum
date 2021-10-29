# boxplots of variables across time and space

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


levels <- c('T', 'A', 'HIX', 'BIX',
            'DOC_mgL',
            'NH4_ugL', 'NO3NO2_ugL', 'SRP_ugL',
            'TN_ugL', 'TP_ugL', 'Chla_ugL',
            'TN_TP', 'DP_TP', 'DN_TN')
labels <- c('T-alloch', 'A-autoch', 'HIX-alloch', 'BIX-autoch',
            'DOC',
            'NH4', 'NO3', 'SRP',
            'TN', 'TP', 'Chl-a',
            'TN_TP', 'DP_TP', 'DN_TN')
names(labels) <- levels
long$variable <- factor(long$variable, levels = levels)

##########################################################################################################################
### boxplots of concentrations
ggplot(data = long, aes(x = variable, y = value, fill = as.factor(Reservoir))) +
  facet_wrap(~variable, scales = 'free') +
  geom_boxplot() +
  geom_point(position=position_jitterdodge(),alpha=0.15) 
  

ggplot(data = long, aes(x = as.factor(distance_from_stream), y = value, fill = as.factor(distance_from_stream))) +
  facet_wrap(~variable, scales = 'free') +
  geom_boxplot() +
  geom_point(position=position_jitterdodge(),alpha=0.2) +
  theme(legend.position = 'none')

ggplot(data = long[long$distance_from_stream>0,], aes(x = as.factor(distance_from_stream), y = value, fill = as.factor(distance_from_stream))) +
  facet_wrap(~variable, scales = 'free') +
  geom_boxplot() +
  geom_point(position=position_jitterdodge(),alpha=0.3) +
  theme(legend.position = 'none')

ggplot(data = long[long$distance_from_stream>0,], aes(x = as.factor(month(Date)), y = value, fill = as.factor(Date))) +
  facet_wrap(~variable, scales = 'free') +
  geom_boxplot() +
  geom_point(position=position_jitterdodge(),alpha=0.3) +
  theme(legend.position = 'none')

ggplot(data = long, aes(x = as.factor(month(Date)), y = value, fill = as.factor(Date))) +
  facet_wrap(~variable, scales = 'free') +
  geom_boxplot() +
  geom_point(position=position_jitterdodge(),alpha=0.2) +
  theme(legend.position = 'none')

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

##### and WITH the stream data
ggplot(data = long, aes(x = as.factor(distance_from_stream), 
                                                      y = value, 
                                                      fill = as.factor(Reservoir))) +
  facet_wrap(~variable, scales = 'free') +
  geom_boxplot() +
  geom_point(position=position_jitterdodge(),alpha=0.3) #+
theme(legend.position = 'none')
#geom_jitter(width=0.1,alpha=0.2)  


ggplot(data = long, aes(x = as.factor(month(Date)), y = value, fill = as.factor(Reservoir))) +
  facet_wrap(~variable, scales = 'free') +
  geom_boxplot() +
  geom_point(position=position_jitterdodge(),alpha=0.2) #+
  theme(legend.position = 'none')



#########################################################################################################################
#### plot CV as boxplot

# calculate heterogeneity within the reservoir (coefficient of variation) for each reservoir on each RC day

CV_time <- data[data$distance_from_stream>0,] %>% group_by(Reservoir, Site) %>% 
  mutate(CV_chl = cv(Chla_ugL, na.rm = TRUE)) %>% 
  mutate(CV_SRP = cv(SRP_ugL, na.rm = TRUE)) %>% 
  mutate(CV_NO3 = cv(NO3NO2_ugL, na.rm = TRUE)) %>% 
  mutate(CV_NH4 = cv(NH4_ugL, na.rm = TRUE)) %>% 
  mutate(CV_TP = cv(TP_ugL, na.rm = TRUE)) %>% 
  mutate(CV_TN = cv(TN_ugL, na.rm = TRUE)) %>% 
  #mutate(CV_TNTP = cv(TN_TP, na.rm = TRUE)) %>% 
  mutate(CV_A = cv(A, na.rm = TRUE)) %>% 
  mutate(CV_T = cv(A, na.rm = TRUE)) %>% 
  mutate(CV_HIX = cv(HIX, na.rm = TRUE)) %>% 
  mutate(CV_BIX = cv(BIX, na.rm = TRUE)) %>% 
  mutate(CV_DOC = cv(DOC_mgL, na.rm = TRUE)) %>% 
  #mutate(CV_DPTP = cv(DP_TP, na.rm = TRUE)) %>% 
  #mutate(CV_DNTN = cv(DN_TN, na.rm = TRUE)) %>% 
  select(Date, Reservoir, distance_from_stream, distance_m, CV_chl:CV_DOC) %>% 
  distinct(Site, Reservoir, .keep_all = TRUE)
CV_time$Date <- as.Date(CV_time$Date)

time_long <- CV_time %>% 
  pivot_longer(CV_chl:CV_DOC, names_to = 'variable', values_to = 'cv')
time_long$axis <- 'time'

levels <- c('CV_T', 'CV_A', 'CV_HIX', 'CV_BIX',
            'CV_DOC',
            'CV_NH4', 'CV_NO3', 'CV_SRP',
            'CV_TN', 'CV_TP', 'CV_chl')
labels <- c('T-alloch', 'A-autoch', 'HIX-alloch', 'BIX-autoch',
            'DOC',
            'NH4', 'NO3', 'SRP',
            'TN', 'TP', 'Chl-a')
names(labels) <- levels
time_long$variable <- factor(time_long$variable, levels = levels)


CV_site <- data[data$distance_from_stream>0,] %>% group_by(Reservoir, Date) %>% 
  mutate(CV_chl = cv(Chla_ugL, na.rm = TRUE)) %>% 
  mutate(CV_SRP = cv(SRP_ugL, na.rm = TRUE)) %>% 
  mutate(CV_NO3 = cv(NO3NO2_ugL, na.rm = TRUE)) %>% 
  mutate(CV_NH4 = cv(NH4_ugL, na.rm = TRUE)) %>% 
  mutate(CV_TP = cv(TP_ugL, na.rm = TRUE)) %>% 
  mutate(CV_TN = cv(TN_ugL, na.rm = TRUE)) %>% 
  #mutate(CV_TNTP = cv(TN_TP, na.rm = TRUE)) %>% 
  mutate(CV_A = cv(A, na.rm = TRUE)) %>% 
  mutate(CV_T = cv(A, na.rm = TRUE)) %>% 
  mutate(CV_HIX = cv(HIX, na.rm = TRUE)) %>% 
  mutate(CV_BIX = cv(BIX, na.rm = TRUE)) %>% 
  mutate(CV_DOC = cv(DOC_mgL, na.rm = TRUE)) %>% 
  #mutate(CV_DPTP = cv(DP_TP, na.rm = TRUE)) %>% 
  #mutate(CV_DNTN = cv(DN_TN, na.rm = TRUE)) %>% 
  select(Date, Reservoir, distance_from_stream, distance_m, CV_chl:CV_DOC) %>% 
  distinct(Date, Reservoir, .keep_all = TRUE)
CV_site$Date <- as.Date(CV_site$Date)

site_long <- CV_site %>% 
  pivot_longer(CV_chl:CV_DOC, names_to = 'variable', values_to = 'cv')
site_long$axis <- 'space'
names(labels) <- levels
site_long$variable <- factor(site_long$variable, levels = levels)

## plots
ggplot(time_long, aes(x = as.factor(Site), y = cv, fill = Reservoir)) +
  geom_bar(position = 'dodge', stat = 'identity') +
  facet_wrap(~variable, scale = 'free') 

ggplot(site_long, aes(x = as.factor(month(Date)), y = cv, fill = Reservoir)) +
  geom_bar(position = 'dodge', stat = 'identity') +
  facet_wrap(~variable, scale = 'free')


# combine
long_both <- full_join(time_long, site_long)

ggplot(long_both, aes(x = as.factor(axis), y = cv, fill = Reservoir)) +
  geom_boxplot() +
  facet_wrap(~variable, scale = 'free') +
  geom_point(position=position_jitterdodge(),alpha=0.3)# +
  ggtitle('just in-reservoir data')
  

####### do again WITH stream data
CV_time <- data %>% group_by(Reservoir, Site) %>% 
  mutate(CV_chl = cv(Chla_ugL, na.rm = TRUE)) %>% 
  mutate(CV_SRP = cv(SRP_ugL, na.rm = TRUE)) %>% 
  mutate(CV_NO3 = cv(NO3NO2_ugL, na.rm = TRUE)) %>% 
  mutate(CV_NH4 = cv(NH4_ugL, na.rm = TRUE)) %>% 
  mutate(CV_TP = cv(TP_ugL, na.rm = TRUE)) %>% 
  mutate(CV_TN = cv(TN_ugL, na.rm = TRUE)) %>% 
  mutate(CV_TNTP = cv(TN_TP, na.rm = TRUE)) %>% 
  mutate(CV_A = cv(A, na.rm = TRUE)) %>% 
  mutate(CV_T = cv(A, na.rm = TRUE)) %>% 
  mutate(CV_HIX = cv(HIX, na.rm = TRUE)) %>% 
  mutate(CV_BIX = cv(BIX, na.rm = TRUE)) %>% 
  mutate(CV_DPTP = cv(DP_TP, na.rm = TRUE)) %>% 
  mutate(CV_DNTN = cv(DN_TN, na.rm = TRUE)) %>% 
  select(Date, Reservoir, distance_from_stream, distance_m, CV_chl:CV_DNTN) %>% 
  distinct(Site, Reservoir, .keep_all = TRUE)
CV_time$Date <- as.Date(CV_time$Date)

time_long <- CV_time %>% 
  pivot_longer(CV_chl:CV_DNTN, names_to = 'variable', values_to = 'cv')
time_long$axis <- 'time'

CV_site <- data %>% group_by(Reservoir, Date) %>% 
  mutate(CV_chl = cv(Chla_ugL, na.rm = TRUE)) %>% 
  mutate(CV_SRP = cv(SRP_ugL, na.rm = TRUE)) %>% 
  mutate(CV_NO3 = cv(NO3NO2_ugL, na.rm = TRUE)) %>% 
  mutate(CV_NH4 = cv(NH4_ugL, na.rm = TRUE)) %>% 
  mutate(CV_TP = cv(TP_ugL, na.rm = TRUE)) %>% 
  mutate(CV_TN = cv(TN_ugL, na.rm = TRUE)) %>% 
  mutate(CV_TNTP = cv(TN_TP, na.rm = TRUE)) %>% 
  mutate(CV_A = cv(A, na.rm = TRUE)) %>% 
  mutate(CV_T = cv(A, na.rm = TRUE)) %>% 
  mutate(CV_HIX = cv(HIX, na.rm = TRUE)) %>% 
  mutate(CV_BIX = cv(BIX, na.rm = TRUE)) %>% 
  mutate(CV_DPTP = cv(DP_TP, na.rm = TRUE)) %>% 
  mutate(CV_DNTN = cv(DN_TN, na.rm = TRUE)) %>% 
  select(Date, Reservoir, distance_from_stream, distance_m, CV_chl:CV_DNTN) %>% 
  distinct(Date, Reservoir, .keep_all = TRUE)
CV_site$Date <- as.Date(CV_site$Date)

site_long <- CV_site %>% 
  pivot_longer(CV_chl:CV_DNTN, names_to = 'variable', values_to = 'cv')
site_long$axis <- 'space'

# combine
long_both <- full_join(time_long, site_long)

ggplot(long_both, aes(x = as.factor(axis), y = cv, fill = Reservoir)) +
  geom_boxplot() +
  facet_wrap(~variable, scale = 'free') +
  geom_point(position=position_jitterdodge(),alpha=0.3) +
  ggtitle('including stream data')
