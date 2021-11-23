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

# some SRP values are 0, so add very very small value to these for dividing by this value later
min_val <- min(data[data$SRP_ugL>0,"SRP_ugL"])
data$SRP_ugL <- data$SRP_ugL + min_val


data <- data %>% 
  mutate(TN_TP = TN_ugL/TP_ugL) %>% 
  mutate(DP_TP = SRP_ugL/TP_ugL) %>% 
  mutate(DN_TN = (NH4_ugL + NO3NO2_ugL)/TN_ugL) %>% 
  mutate(DN_DP = (NH4_ugL + NO3NO2_ugL)/SRP_ugL)


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
  pivot_longer(TN_ugL:DN_DP, names_to = 'variable', values_to = 'value')

vars_keep <-  c('T', 'A', 
                'DOC_mgL',  'Chla_ugL','TN_ugL', 'TP_ugL',
                'NH4_ugL', 'NO3NO2_ugL', 'SRP_ugL') #'HIX', 'BIX'
vars_stoich <-  c('TN_TP', 'DN_DP', 'DP_TP', 'DN_TN')
stoich <- long[long$variable %in% vars_stoich,]

long <- long[long$variable %in% vars_keep,]

levels <- c('T', 'A', 'DOC_mgL',  
            'NH4_ugL', 'NO3NO2_ugL', 'SRP_ugL',
            'Chla_ugL','TN_ugL', 'TP_ugL')
            #'TN_TP', 'DP_TP', 'DN_TN') #'HIX', 'BIX'
labels <- c('T-autoch',  'A-alloch', 'DOC',
            'NH4', 'NO3', 'SRP',
            'Chl-a', 'TN', 'TP') #'BIX-autoch','HIX-alloch',
            #'TN_TP', 'DP_TP', 'DN_TN')
names(labels) <- levels
long$variable <- factor(long$variable, levels = levels)
long$Month <- as.factor(month(long$Date))

######################################################################################################################
### time series color by date, one for each reservoir

## FCR
f <- ggplot(data = long[long$Reservoir=='FCR' & long$distance_from_stream > 0,], aes(x = distance_m, y = value, col = Month)) +
  geom_point(size = 2) +
  geom_smooth(aes(col = Month), alpha = 0.5) +
  xlab('Distance from stream (m)') +
  ylab('Concentration') +
  facet_wrap(~variable, scales = "free_y", labeller = labeller(variable = labels)) + 
  scale_color_manual(values = rev(hcl.colors(7, "Zissou 1"))) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  ggtitle('Falling Creek Reservoir')
# concentration units: add in figure caption?


## BVR
b <- ggplot(data = long[long$Reservoir=='BVR' & long$distance_from_stream > 0,], aes(x = distance_m, y = value, col = Month)) +
  geom_point(size = 2) +
  geom_smooth(aes(col = Month)) +
  xlab('Distance from stream (m)') +
  ylab('Concentration') +
  facet_wrap(~variable, scales = "free_y", labeller = labeller(variable = labels)) + 
  scale_color_manual(values = rev(hcl.colors(7, "Zissou 1"))) +
  theme_bw() +
  labs(col = 'Month') +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none") +
  ggtitle('Beaverdam Reservoir')

ggarrange(b, f, nrow = 1, ncol = 2, common.legend = TRUE, legend = 'right') 


############################################################################################################
### stoiciometry figure

stoich$Month <- as.factor(month(stoich$Date))
levels <- c('TN_TP', 'DN_DP', 'DP_TP', 'DN_TN')
labels <- c('TN_TP', 'DN_DP', 'DP_TP', 'DN_TN')
names(labels) <- levels
stoich$variable <- factor(stoich$variable, levels = levels)


## FCR
fs <- ggplot(data = stoich[stoich$Reservoir=='FCR' & stoich$distance_from_stream > 0,], aes(x = distance_m, y = value, col = Month)) +
  geom_point(size = 2) +
  geom_smooth(aes(col = Month)) +
  xlab('Distance from stream (m)') +
  ylab('Concentration') +
  facet_wrap(~variable, scales = "free_y") + 
  scale_color_manual(values = rev(hcl.colors(7, "Zissou 1"))) +
  theme_bw() +
  labs(col = 'Month') +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none") +
  ggtitle('Falling Creek Reservoir')

## BVR
bs <- ggplot(data = stoich[stoich$Reservoir=='BVR' & stoich$distance_from_stream > 0,], aes(x = distance_m, y = value, col = Month)) +
  geom_point(size = 2) +
  geom_smooth(aes(col = Month)) +
  xlab('Distance from stream (m)') +
  ylab('Concentration') +
  facet_wrap(~variable, scales = "free_y") + 
  scale_color_manual(values = rev(hcl.colors(7, "Zissou 1"))) +
  theme_bw() +
  labs(col = 'Month') +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none") +
  ggtitle('Beaverdam Reservoir')


ggarrange(bs, fs, nrow = 1, ncol = 2, common.legend = TRUE, legend = 'right') 



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
