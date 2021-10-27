
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
write.csv(data, './Data/intermediate_dataset.csv', row.names = FALSE)

long <- data %>%   
  select(Site, Reservoir, Date, distance_from_stream, TN_ugL:Chla_ugL, A:BIX, TN_TP:DN_TN, sp_cond, distance_m) %>% 
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
       temp3$delta[temp3$Site==45] <- temp3$value[temp3$Site==45] - mean(temp3$value[temp3$Site==1], temp3$value[temp3$Site==30], na.rm = TRUE)
       temp3$delta[temp3$Site==50] <- temp3$value[temp3$Site==50] - temp3$value[temp3$Site==45]
     }
     if(temp3$Reservoir=='FCR'){
       temp3$delta[temp3$Site==99] <- 0
       temp3$delta[temp3$Site==200] <- 0
       temp3$delta[temp3$Site==20] <- temp3$value[temp3$Site==20] - mean(temp3$value[temp3$Site==99], temp3$value[temp3$Site==200], na.rm = TRUE)
       temp3$delta[temp3$Site==30] <- temp3$value[temp3$Site==30] - temp3$value[temp3$Site==20]
       temp3$delta[temp3$Site==45] <- temp3$value[temp3$Site==45] - temp3$value[temp3$Site==30]
       temp3$delta[temp3$Site==50] <- temp3$value[temp3$Site==50] - temp3$value[temp3$Site==45]
       
     }  
    test <- rbind(test, temp3)   
   }
  }
}

### delta = what was produced between the two sites, so a positive value indicates that nutrients were produced, 
### negative means they were buried
ggplot(data = test[test$distance_from_stream >0 & test$Reservoir=='BVR',], aes(x = distance_m, y = delta)) +
  facet_wrap(~variable, scales = 'free') +
  geom_line(aes(col = as.factor(Date))) +
  geom_hline(aes(yintercept = 0))

ggplot(data = test[test$distance_from_stream >0 & test$Reservoir=='FCR',], aes(x = distance_m, y = delta)) +
  facet_wrap(~variable, scales = 'free') +
  geom_line(aes(col = as.factor(Date))) +
  geom_hline(aes(yintercept = 0))

