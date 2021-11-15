
library(tidyverse)
library(patchwork)
library(lubridate)
library(EnvStats)
library(ggpubr)
#library(ggpmisc)

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

cond <- read.csv('./Data/rc_temp_conductivity.csv')
cond <- cond %>% 
  select(Reservoir, DateTime, Site, Sp.Cond...Micro.S., Notes) %>% 
  rename(sp_cond = Sp.Cond...Micro.S.)
cond$Date <- as.Date(cond$DateTime)
cond$Site <- as.numeric(gsub("[A-z]", "", cond$Site)) # Remove letters
cond$Letter <- gsub("[0-9]", "", cond$Site) # Remove numbers
cond <- cond %>% 
  select(Site, Reservoir, Date, sp_cond)

data <- left_join(data, cond)
#write.csv(data, './Data/intermediate_dataset.csv', row.names = FALSE)

long <- data %>%   
  select(Site, Reservoir, Date, Flow_cms, distance_from_stream, TN_ugL:Chla_ugL, A:BIX, TN_TP:DN_TN, sp_cond, distance_m) %>% 
  pivot_longer(TN_ugL:sp_cond, names_to = 'variable', values_to = 'value')

# calculate 'processing'
vars <- unique(long$variable)
dates <- unique(long$Date)
res <- unique(long$Reservoir)
dates <- dates[-6]

test <- NA

for(j in 1:length(vars)){
 temp <- long[long$variable==vars[j],]
 for (k in 1:length(dates)) {
   temp2 <- temp[temp$Date==dates[k],]
   for(m in 1:length(res)){
     temp3 <- temp2[temp2$Reservoir==res[m],]
     if(temp3$Reservoir=='BVR'){
       temp3$delta[temp3$Site==100] <- 0
       temp3$delta[temp3$Site==200] <- 0
       temp3$delta[temp3$Site==20] <- temp3$value[temp3$Site==20] - temp3$value[temp3$Site==100]
       temp3$delta[temp3$Site==30] <- temp3$value[temp3$Site==30] - temp3$value[temp3$Site==200]
       temp3$delta[temp3$Site==1] <- temp3$value[temp3$Site==1] - temp3$value[temp3$Site==20]
       temp3$delta[temp3$Site==45] <- temp3$value[temp3$Site==45] - mean(c(temp3$value[temp3$Site==1], temp3$value[temp3$Site==30]), na.rm = TRUE)
       #temp3$delta[temp3$Site==45] <- temp3$value[temp3$Site==45] - (temp3$value[temp3$Site==1] - temp3$value[temp3$Site==30])
       temp3$delta[temp3$Site==50] <- temp3$value[temp3$Site==50] - temp3$value[temp3$Site==45]
       #temp3$delta_simple <- temp3$value[temp3$Site==50] - mean(c(temp3$value[temp3$Site==100], temp3$value[temp3$Site==200]), na.rm = TRUE)
       temp3$delta_simple <- temp3$value[temp3$Site==50] - temp3$value[temp3$Site==100] - temp3$value[temp3$Site==200]
       #temp3$delta_cumu <- sum(temp3$delta, na.rm = TRUE)
     }
     if(temp3$Reservoir=='FCR'){
       temp3$delta[temp3$Site==99] <- 0
       temp3$delta[temp3$Site==200] <- 0
       temp3$delta[temp3$Site==20] <- temp3$value[temp3$Site==20] - (temp3$value[temp3$Site==99] - temp3$value[temp3$Site==200])
       #temp3$delta[temp3$Site==20] <- temp3$value[temp3$Site==20] - mean(c(temp3$value[temp3$Site==99], temp3$value[temp3$Site==200]), na.rm = TRUE)
       temp3$delta[temp3$Site==30] <- temp3$value[temp3$Site==30] - temp3$value[temp3$Site==20]
       temp3$delta[temp3$Site==45] <- temp3$value[temp3$Site==45] - temp3$value[temp3$Site==30]
       temp3$delta[temp3$Site==50] <- temp3$value[temp3$Site==50] - temp3$value[temp3$Site==45]
       #temp3$delta_simple <- temp3$value[temp3$Site==50] - mean(c(temp3$value[temp3$Site==99], temp3$value[temp3$Site==200]), na.rm = TRUE)
       temp3$delta_simple <- temp3$value[temp3$Site==50] - temp3$value[temp3$Site==99] - temp3$value[temp3$Site==200]
       #temp3$cumu <- sum(temp3$delta, na.rm = TRUE)
       
       
     }  
    test <- rbind(test, temp3)   
   }
  }
}

test <- test[-1,]

### delta = what was produced between the two sites, so a positive value indicates that nutrients were produced, 
### negative means they were buried

vars_keep <- c('Chla_ugL', 'DOC_mgL', 'NH4_ugL', 'NO3NO2_ugL', 'SRP_ugL', 'TN_ugL', 'TP_ugL', 'sp_cond')
test <- test[test$variable %in% vars_keep,]

# add LOQ values for each analyte: https://docs.google.com/spreadsheets/d/12jLdBMZKTm7Om8Wyo4L_yNjLb_2fCIGw/edit#gid=1973039800
test$loq <- NA
test$loq[test$variable=='NH4_ugL'] <- 11.8
test$loq[test$variable=='SRP_ugL'] <- 6.2
test$loq[test$variable=='NO3NO2_ugL'] <- 9.3
test$loq[test$variable=='TP_ugL'] <- 13.2
test$loq[test$variable=='TN_ugL'] <- 67.2
test$loq[test$variable=='DOC_mgL'] <- 0.411

# because all values are less than 50, the accuracy is 1% of reading +/- 0.05 uS/cm
# taken from manual for conductivity sensor
sp <- test[test$distance_from_stream > 0 & test$variable=='sp_cond',]
hist(sp$value)

for(i in 1:nrow(test)){
  if(test$variable[i]=='sp_cond'){
    test$loq[i] <- 0.01*test$value[i] + 0.05
    test$delta_simple[i] <- NA
    
  }
}

ggplot(data = test[test$distance_from_stream >0 & test$Reservoir=='BVR',], aes(x = distance_m, y = delta)) +
  facet_wrap(~variable, scales = 'free') +
  geom_line(aes(col = as.factor(month(Date)))) +
  geom_hline(aes(yintercept = 0)) +
  geom_point(aes(x = 1100, y = delta_simple, col = as.factor(month(Date))), size = 4)+
  geom_ribbon(aes(ymin = delta -loq, ymax = delta + loq, col = as.factor(month(Date))), alpha = 0.1, linetype = 0)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab('Delta from upstream') +
  xlab('Distance from upstream (m)')


ggplot(data = test[test$distance_from_stream >0 & test$Reservoir=='FCR',], aes(x = distance_m, y = delta)) +
  facet_wrap(~variable, scales = 'free') +
  geom_line(aes(col = as.factor(month(Date)))) +
  geom_hline(aes(yintercept = 0)) +
  geom_point(aes(x = 600, y = delta_simple, col = as.factor(month(Date))), size = 4)+
  geom_ribbon(aes(ymin = delta -loq, ymax = delta + loq, col = as.factor(month(Date))), alpha = 0.1, linetype = 0)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  ylab('Delta from upstream') +
  xlab('Distance from upstream (m)')


##### calculate normalized distance 
test <- test %>% 
  mutate(delta_norm = (delta/distance_m)) %>% 
  mutate(norm_distance = ifelse(Reservoir=='BVR', distance_m/1117, distance_m/642))

ggplot(data = test[test$distance_from_stream>0,], aes(x = distance_m, y = delta_norm)) +
  facet_wrap(~variable, scales = 'free') +
  geom_line(aes(col = as.factor(month(Date)), linetype = Reservoir)) +
  geom_hline(aes(yintercept = 0)) +
  #geom_point(aes(x = 600, y = delta_simple, col = as.factor(month(Date))), size = 4) +
  ylab('Delta over distance (ug/L / m)') +
  xlab('Normalized distance (% of total reservoir length)')
  
ggplot(data = test[test$distance_from_stream>0,], aes(x = norm_distance, y = delta_norm)) +
  facet_wrap(~variable, scales = 'free') +
  geom_line(aes(col = as.factor(month(Date)), linetype = Reservoir)) +
  geom_hline(aes(yintercept = 0))

ggplot(data = test[test$distance_from_stream>0 & test$Reservoir=='FCR',], aes(x = distance_m, y = delta_norm)) +
  facet_wrap(~variable, scales = 'free') +
  geom_line(aes(col = as.factor(month(Date)))) +
  geom_hline(aes(yintercept = 0))
  
ggplot(data = test[test$distance_from_stream>0 & test$Reservoir=='BVR',], aes(x = distance_m, y = delta_norm)) +
  facet_wrap(~variable, scales = 'free') +
  geom_line(aes(col = as.factor(month(Date)))) +
  geom_hline(aes(yintercept = 0)) 
  
  
# now calculate q*concentration
load_sites <- c('FCR_99', 'FCR_200', 'BVR_100', 'BVR_200', 'FCR_1',  'FCR_50', 'BVR_50')
bvr_res <- c('BVR_20', 'BVR_30', 'BVR_1', 'BVR_45', 'BVR_50')
fcr_res <- c('FCR_20', 'FCR_30', 'FCR_45', 'FCR_50')

long$site_res <- paste0(long$Reservoir, "_", long$Site)
long_load <- long


# assign flows for F50 based on F01 (but not use concentrations from F01 because they come from toe drain sometimes)
for(i in 1:length(unique(long_load$Date))){
  if(length(long_load$Flow_cms[long_load$site_res=='FCR_1'& long_load$Date==unique(long_load$Date)[i]]) > 1){
  long_load$Flow_cms[long_load$site_res %in% fcr_res & long_load$Date==unique(long_load$Date)[i]] <- long_load$Flow_cms[long_load$site_res=='FCR_1'& long_load$Date==unique(long_load$Date)[i]]
  }
}


# need to add in flows for B50 from BVR GLM outflow estimates: https://github.com/CareyLabVT/BVR-GLM/blob/master/inputs/BVR_spillway_outflow_2014_2019_20210223_nldasInflow.csv
long_load$Flow_cms[long_load$site_res %in% bvr_res & long_load$Date=='2019-04-29'] <- 0.0204
long_load$Flow_cms[long_load$site_res %in% bvr_res & long_load$Date=='2019-05-30'] <- 0.0329
long_load$Flow_cms[long_load$site_res %in% bvr_res & long_load$Date=='2019-06-27'] <- 0.0126
long_load$Flow_cms[long_load$site_res %in% bvr_res & long_load$Date=='2019-07-18'] <- 0.0016
long_load$Flow_cms[long_load$site_res %in% bvr_res & long_load$Date=='2019-08-22'] <- 0.0173
long_load$Flow_cms[long_load$site_res %in% bvr_res & long_load$Date=='2019-09-20'] <- 0.005
long_load$Flow_cms[long_load$site_res %in% bvr_res & long_load$Date=='2019-10-04'] <- 0.0097


long_load <- long_load %>% 
  group_by(Date, Reservoir, Site) %>% 
  mutate(load = value*Flow_cms)


# use loads to calculate the delta concentration instead of raw values
long_load$delta_load_simple <- NA
long_load$delta_load_spatial <- NA
long_load_2 <- long_load

test_load <- NA

for(j in 1:length(vars)){
  temp <- long_load_2[long_load_2$variable==vars[j],]
  for (k in 1:length(dates)) {
    temp2 <- temp[temp$Date==dates[k],]
    for(m in 1:length(res)){
      temp3 <- temp2[temp2$Reservoir==res[m],]
      if(temp3$Reservoir=='BVR'){
        temp3$delta_load_spatial[temp3$Site==100] <- 0
        temp3$delta_load_spatial[temp3$Site==200] <- 0
        temp3$delta_load_spatial[temp3$Site==20] <- temp3$load[temp3$Site==20] - temp3$load[temp3$Site==100]
        temp3$delta_load_spatial[temp3$Site==30] <- temp3$load[temp3$Site==30] - temp3$load[temp3$Site==200]
        temp3$delta_load_spatial[temp3$Site==1] <- temp3$load[temp3$Site==1] - temp3$load[temp3$Site==20]
        temp3$delta_load_spatial[temp3$Site==45] <- temp3$load[temp3$Site==45] - mean(c(temp3$load[temp3$Site==1], temp3$load[temp3$Site==30]), na.rm = TRUE)
        #temp3$delta_load_spatial[temp3$Site==45] <- temp3$load[temp3$Site==45] - (temp3$load[temp3$Site==1] - temp3$load[temp3$Site==30])
        temp3$delta_load_spatial[temp3$Site==50] <- temp3$load[temp3$Site==50] - temp3$load[temp3$Site==45]
        #temp3$delta_load_simple <- temp3$load[temp3$Site==50] - mean(c(temp3$load[temp3$Site==100], temp3$load[temp3$Site==200]), na.rm = TRUE)
        temp3$delta_load_simple <- temp3$load[temp3$Site==50] - sum(temp3$load[temp3$Site==100], temp3$load[temp3$Site==200], na.rm = TRUE)
        
      }
      if(temp3$Reservoir=='FCR'){
        temp3$delta_load_spatial[temp3$Site==99] <- 0
        temp3$delta_load_spatial[temp3$Site==200] <- 0
        temp3$delta_load_spatial[temp3$Site==20] <- temp3$load[temp3$Site==20] - sum(temp3$load[temp3$Site==99], temp3$load[temp3$Site==200], na.rm = TRUE)
        #temp3$delta_load_spatial[temp3$Site==20] <- temp3$load[temp3$Site==20] - mean(c(temp3$load[temp3$Site==99], temp3$load[temp3$Site==200]), na.rm = TRUE)
        temp3$delta_load_spatial[temp3$Site==30] <- temp3$load[temp3$Site==30] - temp3$load[temp3$Site==20]
        temp3$delta_load_spatial[temp3$Site==45] <- temp3$load[temp3$Site==45] - temp3$load[temp3$Site==30]
        temp3$delta_load_spatial[temp3$Site==50] <- temp3$load[temp3$Site==50] - temp3$load[temp3$Site==45]
        #temp3$delta_load_simple <- temp3$load[temp3$Site==50] - mean(c(temp3$load[temp3$Site==99], temp3$load[temp3$Site==200]), na.rm = TRUE)
        temp3$delta_load_simple <- temp3$load[temp3$Site==50] - sum(temp3$load[temp3$Site==99], temp3$load[temp3$Site==200], na.rm = TRUE)
        
        
      }  
      test_load <- rbind(test_load, as.data.frame(temp3))   
    }
  }
}

test_load <- test_load[-1,]
exclude <- c('FCR_102', 'FCR_101', 'FCR_100', 'FCR_1')
test_load <- test_load[!(test_load$site_res %in% exclude),]

# add LOQ values for each analyte: https://docs.google.com/spreadsheets/d/12jLdBMZKTm7Om8Wyo4L_yNjLb_2fCIGw/edit#gid=1973039800
test_load$loq <- NA
test_load$loq[test_load$variable=='NH4_ugL'] <- 11.8
test_load$loq[test_load$variable=='SRP_ugL'] <- 6.2
test_load$loq[test_load$variable=='NO3NO2_ugL'] <- 9.3
test_load$loq[test_load$variable=='TP_ugL'] <- 13.2
test_load$loq[test_load$variable=='TN_ugL'] <- 67.2
test_load$loq[test_load$variable=='DOC_mgL'] <- 0.411

# because all values are less than 50, the accuracy is 1% of reading +/- 0.05 uS/cm
# taken from manual for conductivity sensor
sp <- test_load[test_load$distance_from_stream > 0 & test_load$variable=='sp_cond',]
hist(sp$value)

for(i in 1:nrow(test_load)){
  if(test_load$variable[i]=='sp_cond'){
    test_load$loq[i] <- 0.01*test_load$value[i] + 0.05
  }
}



ggplot(data = test_load[test_load$distance_from_stream >0 & test_load$Reservoir=='BVR',], aes(x = distance_m, y = delta_load_spatial)) +
  facet_wrap(~variable, scales = 'free') +
  geom_line(aes(col = as.factor(month(Date)))) +
  geom_hline(aes(yintercept = 0)) +
  geom_point(aes(x = 1100, y = delta_load_simple, col = as.factor(month(Date))), size = 4)+
  geom_ribbon(aes(ymin = delta_load_spatial -loq*Flow_cms, ymax = delta_load_spatial + loq*Flow_cms, col = as.factor(month(Date))), alpha = 0.1, linetype = 0)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab('Delta load from upstream') +
  xlab('Distance from upstream (m)')


ggplot(data = test_load[test_load$distance_from_stream >0 & test_load$Reservoir=='FCR',], aes(x = distance_m, y = delta_load_spatial)) +
  facet_wrap(~variable, scales = 'free') +
  geom_line(aes(col = as.factor(month(Date)))) +
  geom_hline(aes(yintercept = 0)) +
  geom_point(aes(x = 600, y = delta_load_simple, col = as.factor(month(Date))), size = 4)+
  geom_ribbon(aes(ymin = delta_load_spatial -loq*Flow_cms, ymax = delta_load_spatial + loq*Flow_cms, col = as.factor(month(Date))), alpha = 0.1, linetype = 0)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  ylab('Delta load from upstream') +
  xlab('Distance from upstream (m)')


# how do BVR and FCR flow quantities compare
inf <- long_load_2[long_load_2$Site > 50,]
inf <- inf %>% 
  select(Site, Reservoir, Date, site_res, Flow_cms) %>% 
  distinct(Site, Reservoir, Date, .keep_all = TRUE) %>% 
  group_by(Reservoir, Date) %>% 
  mutate(cum_inf = sum(Flow_cms, na.rm = TRUE))

ggplot(data = inf, aes(x = Date, y = cum_inf, col = Reservoir)) +
  geom_line()
ggplot(data = inf, aes(x = Date, y = Flow_cms, col = as.factor(site_res))) +
  geom_line()
