# plot discharge and specific conductivity time series
#install.packages('ggrepel')

library(tidyverse)
library(patchwork)
library(lubridate)
library(EnvStats)
library(ggpubr)
library(ggrepel)
#library(ggpmisc)

r_col <- c('olivedrab3', 'royalblue1')


#data <- read.csv('./Data/continuum_pf.csv')
data <-read.csv('./Data/continuum_data.csv')
data$Date <- as.Date(data$Date)



# keep only inflow sites used in analysis
data$res_site <- paste0(data$Reservoir, "_", data$Site)
load_sites <- c('FCR_99', 'FCR_200', 'BVR_100', 'BVR_200', 'FCR_1',  'FCR_50', 'BVR_50')
bvr_res <- c('BVR_20', 'BVR_30', 'BVR_1', 'BVR_45', 'BVR_50')


flow <- data[data$res_site %in% load_sites,]
flow <- flow %>% dplyr::select(Date, res_site, Reservoir, Site, Flow_cms, distance_from_stream)
data <- data[!(data$res_site %in% c('FCR_100', 'FCR_102', 'FCR_101', 'FCR_1')),]

# remove september date because we don't have inflow data
flow <- flow[flow$Date!='2019-09-20' & flow$Date!='2019-07-22',]

# assign flows for F50 based on F01 
flow$Flow_cms[flow$res_site=='FCR_50'] <- flow$Flow_cms[flow$res_site=='FCR_1']

# need to add in flows for B50 from BVR GLM outflow estimates: https://github.com/CareyLabVT/BVR-GLM/blob/master/inputs/BVR_spillway_outflow_2014_2019_20210223_nldasInflow.csv
flow$Flow_cms[flow$res_site %in% bvr_res & flow$Date=='2019-04-29'] <- 0.0204
flow$Flow_cms[flow$res_site %in% bvr_res & flow$Date=='2019-05-30'] <- 0.0329
flow$Flow_cms[flow$res_site %in% bvr_res & flow$Date=='2019-06-27'] <- 0.0126
flow$Flow_cms[flow$res_site %in% bvr_res & flow$Date=='2019-07-18'] <- 0.0016
flow$Flow_cms[flow$res_site %in% bvr_res & flow$Date=='2019-08-22'] <- 0.0173
flow$Flow_cms[flow$res_site %in% bvr_res & flow$Date=='2019-09-20'] <- 0.005
flow$Flow_cms[flow$res_site %in% bvr_res & flow$Date=='2019-10-04'] <- 0.0097

flow <- flow[flow$res_site!='FCR_1',]
data <- data[data$res_site!='FCR_1',]

data$location <- NA
stream_sites <- c('FCR_200', 'FCR_99', 'BVR_100', 'BVR_200')
reservoir_sites <- c( 'BVR_20', 'BVR_30','BVR_1', 'BVR_45', 'BVR_50',
                      'FCR_20', 'FCR_30', 'FCR_45', 'FCR_50') # site 01 is in res for BVR and in stream for FCR 

for(i in 1:nrow(data)){
  if(data$res_site[i] %in% reservoir_sites){
    data$location[i] <- 'Reservoir'
  }else{
    data$location[i] <- 'Stream'
  }
}
data$Site <- as.factor(data$Site)
data$location <- as.factor(data$location)
flow <- flow %>% 
  mutate(name = ifelse(res_site=="FCR_99", '100', Site)) %>% 
  mutate(labels = as.factor(Site))

inf <- ggplot(data = flow, aes(x = Date, y = Flow_cms)) +
  geom_point(aes(col = Reservoir, shape = labels), size = 4) +
  geom_line(data = flow[flow$Reservoir=='FCR',], aes(shape = labels, col = Reservoir)) +
  geom_line(data = flow[flow$Reservoir=='BVR',], aes(shape = labels, col = Reservoir)) +
  theme_bw() +
  scale_color_manual(values = r_col) +
  #scale_shape_manual(values = c(21,19)) +
  ylab('Discharge (m3/s)') +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  labs(color = 'Site', size = NULL)
inf

spc <-  ggplot(data = data, aes(x = Date, y = Sp_cond_uScm)) +
  geom_point(aes(col = as.factor(Site), shape = as.factor(location)), size = 4) +
  geom_line(aes(col = as.factor(Site))) +
  facet_wrap(~Reservoir) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  ylab('Specific Conductivity (us/cm)') +
  labs(color = 'Site', shape = 'Location')
  #geom_text(data = data[data$location=='Stream',], aes(x = Date, y = Sp_cond_uScm_uScm, label = Site),
  #          nudge_y = 1)
spc
ggarrange(inf, spc, nrow = 1, ncol = 2, legend = 'right') 

data <- data %>% 
  mutate(labels = Site)

spc <-  ggplot(data = data, aes(x = distance_m, y = Sp_cond_uScm)) +
  geom_point(data = data[data$distance_from_stream > 0,], aes(col = as.factor(month(Date)), size = 2)) +
  geom_point(data = data[data$distance_from_stream < 1,], aes(col = as.factor(month(Date)), size = 2, shape = res_site)) +
  geom_line(data = data[data$Site!='200',], aes(col = as.factor(month(Date)))) +
  geom_line(data = data[data$Site!='100' & data$Site!='99',], aes(col = as.factor(month(Date)))) +
  facet_wrap(~Reservoir, scales = 'free_x') +
  #geom_label(aes(label = labels), na.rm = TRUE) +
  scale_size(guide = 'none') +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  ylab('Specific Conductivity (us/cm)') +
  labs(color = 'Month', shape = 'Location', size = "")+
  scale_color_manual(values = rev(hcl.colors(7, "Zissou 1")), name = 'Month') 
  
spc

ggarrange(inf, spc, nrow = 1, ncol = 2, legend = 'right') 
