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

# add the simple site labels
flow$simple_site <- NA
flow$simple_site <- factor(flow$simple_site, levels = c('S1', 'S2', 'Out'))
for(i in 1:nrow(flow)){
  if(flow$res_site[i]=='BVR_100'){
    flow$simple_site[i] <- 'S1'
  }
  if(flow$res_site[i]=='BVR_200'){
    flow$simple_site[i] <- 'S2'
  }
  if(flow$res_site[i]=='BVR_50'){
    flow$simple_site[i] <- 'Out'
  }
  if(flow$res_site[i]=='FCR_99'){
    flow$simple_site[i] <- 'S2'
  }
  if(flow$res_site[i]=='FCR_200'){
    flow$simple_site[i] <- 'S1'
  }
  if(flow$res_site[i]=='FCR_50'){
    flow$simple_site[i] <- 'Out'
  }
}

inf <- ggplot(data = flow, aes(x = Date, y = Flow_cms)) +
  geom_point(aes(col = Reservoir, shape = simple_site), size = 4) +
  geom_line(data = flow[flow$Reservoir=='FCR',], aes(shape = simple_site, col = Reservoir)) +
  geom_line(data = flow[flow$Reservoir=='BVR',], aes(shape = simple_site, col = Reservoir)) +
  facet_wrap(~Reservoir) +
  theme_bw() +
  scale_color_manual(values = r_col) +
  scale_shape_manual(values = c(15, 17, 16)) +
  ylab(expression(Discharge~(m^3/s))) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text = element_text(size = 15),
        axis.text.x = element_text(angle = 45)) +
  labs(shape = 'Site', color = 'Reservoir', size = NULL)
inf



data <- data %>% 
  mutate(labels = Site)

# add the simple site labels
data$simple_site <- NA
data$simple_site <- factor(data$simple_site, levels = c('S1', 'S2', 'In-Reservoir'))
for(i in 1:nrow(data)){
  if(data$res_site[i]=='BVR_100'){
    data$simple_site[i] <- 'S1'
  }
  if(data$res_site[i]=='BVR_200'){
    data$simple_site[i] <- 'S2'
  }
  if(data$res_site[i] %in% c('BVR_1', 'BVR_20', 'BVR_30', 'BVR_45', 'BVR_50',
                             'FCR_20', 'FCR_30', 'FCR_45', 'FCR_50')){
    data$simple_site[i] <- 'In-Reservoir'
  }
  if(data$res_site[i]=='FCR_99'){
    data$simple_site[i] <- 'S2'
  }
  if(data$res_site[i]=='FCR_200'){
    data$simple_site[i] <- 'S1'
  }
}

# calculate avg diff btw stream and in-reservoir sites
diff <- data %>% 
  select(Reservoir, Site, location, Sp_cond_uScm) %>% 
  group_by(location, Reservoir) %>% 
  mutate(mean_spc = mean(Sp_cond_uScm, na.rm = TRUE)) #%>% 
  distinct(location, Date, Reservoir, .keep_all = TRUE) %>% 
  select(Date, Reservoir, location, mean_spc)
  


spc <-  ggplot(data = data, aes(x = distance_m, y = Sp_cond_uScm)) +
  geom_point(data = data[data$distance_from_stream > 0,], aes(col = as.factor(month(Date))), size = 4) +
  geom_point(data = data[data$distance_from_stream < 1,], aes(col = as.factor(month(Date)), shape = simple_site), size = 4) +
  geom_line(data = data[data$Site!='200',], aes(col = as.factor(month(Date)))) +
  geom_line(data = data[data$Site!='100' & data$Site!='99',], aes(col = as.factor(month(Date)))) +
  facet_wrap(~Reservoir, scales = 'free_x') +
  #geom_label(aes(label = labels), na.rm = TRUE) +
  scale_size(guide = 'none') +
  theme_bw() +
  scale_shape_manual(values = c(15, 17, 16)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text = element_text(size = 15)) +
  ylab('Specific Conductance (μs/cm)') +
  xlab('Distance (m)') +
  labs(color = 'Month', shape = 'Site', size = "")+
  scale_color_manual(values = rev(hcl.colors(7, "Zissou 1")), name = 'Month') 
  
spc

sp_flow <- ggarrange(inf, spc, nrow = 1, ncol = 2, legend = 'right', labels = c('a', 'b')) 
sp_flow
ggsave('./Figures/Fig4_spcond_flow.png', sp_flow)

##################################################################################################################
# other spc plot
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
