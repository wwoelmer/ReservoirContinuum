
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
  select(Site, Reservoir, Date, Flow_cms, distance_from_stream, TN_ugL:Chla_ugL, A:BIX, sp_cond, distance_m) %>% 
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
       temp3$delta[temp3$Site==50] <- temp3$value[temp3$Site==50] - temp3$value[temp3$Site==45]
       temp3$delta_simple <- temp3$value[temp3$Site==50] - mean(c(temp3$value[temp3$Site==100], temp3$value[temp3$Site==200]), na.rm = TRUE)
       #temp3$delta_cumu <- sum(temp3$delta, na.rm = TRUE)
     }
     if(temp3$Reservoir=='FCR'){
       temp3$delta[temp3$Site==99] <- 0
       temp3$delta[temp3$Site==200] <- 0
       temp3$delta[temp3$Site==20] <- temp3$value[temp3$Site==20] - mean(c(temp3$value[temp3$Site==99], temp3$value[temp3$Site==200]), na.rm = TRUE)
       temp3$delta[temp3$Site==30] <- temp3$value[temp3$Site==30] - temp3$value[temp3$Site==20]
       temp3$delta[temp3$Site==45] <- temp3$value[temp3$Site==45] - temp3$value[temp3$Site==30]
       temp3$delta[temp3$Site==50] <- temp3$value[temp3$Site==50] - temp3$value[temp3$Site==45]
       temp3$delta_simple <- temp3$value[temp3$Site==50] - mean(c(temp3$value[temp3$Site==99], temp3$value[temp3$Site==200]), na.rm = TRUE)
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
ggplot(data = test[test$distance_from_stream >0 & test$Reservoir=='BVR',], aes(x = distance_m, y = delta)) +
  facet_wrap(~variable, scales = 'free') +
  geom_line(aes(col = as.factor(month(Date)))) +
  geom_hline(aes(yintercept = 0)) +
  geom_point(aes(x = 1100, y = delta_simple, col = as.factor(month(Date))), size = 4)

ggplot(data = test[test$distance_from_stream >0 & test$Reservoir=='FCR',], aes(x = distance_m, y = delta)) +
  facet_wrap(~variable, scales = 'free') +
  geom_line(aes(col = as.factor(month(Date)))) +
  geom_hline(aes(yintercept = 0)) +
  geom_point(aes(x = 600, y = delta_simple, col = as.factor(month(Date))), size = 4)



#
out <- test %>% 
  select(Reservoir, Date, variable, delta_simple, cumu) %>% 
  distinct(Reservoir, Date, variable, .keep_all = TRUE) %>% 
  pivot_longer(delta_simple:cumu, names_to = 'type', values_to = 'delta')

ggplot(data = out[out$Reservoir=='FCR',], aes(x = Date, y = delta, fill = type)) + 
  geom_bar(position = 'dodge', stat = 'identity') +
  facet_wrap(~variable, scales = 'free')

ggplot(data = out[out$Reservoir=='BVR',], aes(x = Date, y = delta, fill = type)) + 
  geom_bar(position = 'dodge', stat = 'identity') +
  facet_wrap(~variable, scales = 'free')

# now calculate q*concentration
load_sites <- c('FCR_99', 'FCR_200', 'BVR_100', 'BVR_200', 'FCR_1',  'FCR_50', 'BVR_50')

long$site_res <- paste0(long$Reservoir, "_", long$Site)
long_load <- long[long$site_res %in% load_sites,]


# assign flows for F50 based on F01 (but not use concentrations from F01 because they come from toe drain sometimes)
long_load$Flow_cms[long_load$site_res=='FCR_50'] <- long_load$Flow_cms[long_load$site_res=='FCR_1']

# need to add in flows for B50 from BVR GLM outflow estimates: https://github.com/CareyLabVT/BVR-GLM/blob/master/inputs/BVR_spillway_outflow_2014_2019_20210223_nldasInflow.csv
long_load$Flow_cms[long_load$site_res=='BVR_50' & long_load$Date=='2019-04-29'] <- 0.0204
long_load$Flow_cms[long_load$site_res=='BVR_50' & long_load$Date=='2019-05-30'] <- 0.0329
long_load$Flow_cms[long_load$site_res=='BVR_50' & long_load$Date=='2019-06-27'] <- 0.0126
long_load$Flow_cms[long_load$site_res=='BVR_50' & long_load$Date=='2019-07-18'] <- 0.0016
long_load$Flow_cms[long_load$site_res=='BVR_50' & long_load$Date=='2019-08-22'] <- 0.0173
long_load$Flow_cms[long_load$site_res=='BVR_50' & long_load$Date=='2019-09-20'] <- 0.005
long_load$Flow_cms[long_load$site_res=='BVR_50' & long_load$Date=='2019-10-04'] <- 0.0097


long_load <- long_load %>% 
  group_by(Date, Reservoir, Site) %>% 
  mutate(load = value*Flow_cms)

long_load$delta_load <- NA
long_load_2 <- long_load
for(j in 1:length(vars)){
  temp <- long_load[long_load$variable==vars[j],]
  for (k in 1:length(dates)) {
    temp2 <- temp[temp$Date==dates[k],]
    for(m in 1:length(res)){
      temp3 <- temp2[temp2$Reservoir==res[m],]
      if(temp3$Reservoir=='BVR'){
       temp3$delta_load <- temp3$load[temp3$site_res=='BVR_50'] - sum(c(temp3$load[temp3$site_res=='BVR_100'], temp3$load[temp3$site_res=='BVR_200']))
      }
      if(temp3$Reservoir=='FCR'){
       temp3$delta_load <- temp3$load[temp3$site_res=='FCR_50'] - sum(c(temp3$load[temp3$site_res=='FCR_99'], temp3$load[temp3$site_res=='FCR_200']))
        
        
      }  
      long_load_2 <- rbind(long_load_2, temp3)   
    }
  }
}



ggplot(data = long_load_2, aes(x = month(Date), y = delta_load, col = as.factor(Reservoir))) +
  geom_point() +
  facet_wrap(~variable, scales = 'free') +
  geom_hline(aes(yintercept = 0))

