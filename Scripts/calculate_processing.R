
library(tidyverse)
library(patchwork)
library(lubridate)
library(EnvStats)
library(ggpubr)
#library(ggpmisc)

r_col <- c('olivedrab3', 'royalblue1')

data <-read.csv('./Data/continuum_data.csv')
data$Date <- as.Date(data$Date)

long <- data %>%   
  select(Site, Reservoir, Date, distance_from_stream, distance_m, Flow_cms, TN_ugL:T, Sp_cond_uScm) %>% 
  pivot_longer(TN_ugL:Sp_cond_uScm, names_to = 'variable', values_to = 'value')

# calculate 'processing'
vars <- unique(long$variable)
dates <- c("2019-04-29",
           "2019-05-30",
           "2019-06-27",
           "2019-07-18",
           "2019-08-22",
           "2019-10-04")
# all dates except Sept 20 2019 because we did not sample inflows this day
res <- unique(long$Reservoir)



### simple concentration delta 
test <- NA

for(j in 1:length(vars)){
 temp <- long[long$variable==vars[j],]
 for (k in 1:length(dates)) {
   temp2 <- temp[temp$Date==dates[k],]
   for(m in 1:length(res)){
     temp3 <- temp2[temp2$Reservoir==res[m],]
     temp3 <- temp3 %>% distinct(temp3$Site, .keep_all = TRUE)
     
     if(temp3$Reservoir[1]=='BVR'){
       # calculate flow-weighting of inflows
       tot_flow <- temp3$Flow_cms[temp3$Site==100] + temp3$Flow_cms[temp3$Site==200] 
       prop_100 <- temp3$Flow_cms[temp3$Site==100]/tot_flow
       prop_200 <- temp3$Flow_cms[temp3$Site==200]/tot_flow
       
       
       temp3$delta[temp3$Site==100] <- 0
       temp3$delta[temp3$Site==200] <- 0
       temp3$delta[temp3$Site==20] <- temp3$value[temp3$Site==20] - temp3$value[temp3$Site==100]
       temp3$delta[temp3$Site==30] <- temp3$value[temp3$Site==30] - temp3$value[temp3$Site==200]
       temp3$delta[temp3$Site==1] <- temp3$value[temp3$Site==1] - temp3$value[temp3$Site==20]
       #temp3$delta[temp3$Site==45] <- temp3$value[temp3$Site==45] - mean(c(temp3$value[temp3$Site==1], temp3$value[temp3$Site==30]), na.rm = TRUE)
       #temp3$delta[temp3$Site==45] <- temp3$value[temp3$Site==45] - (temp3$value[temp3$Site==1]) - (temp3$value[temp3$Site==30])
       temp3$delta[temp3$Site==45] <- temp3$value[temp3$Site==45] - (temp3$value[temp3$Site==1]*prop_100) - (temp3$value[temp3$Site==30]*prop_200)
       temp3$delta[temp3$Site==50] <- temp3$value[temp3$Site==50] - temp3$value[temp3$Site==45]
       temp3$delta_simple <- temp3$value[temp3$Site==50] - temp3$value[temp3$Site==100] - temp3$value[temp3$Site==200]
       temp3$delta_norm_simple <- temp3$delta_simple/1117
       
       ## and calculate the delta distance
       temp3$delta_distance[temp3$Site==100] <- 0
       temp3$delta_distance[temp3$Site==200] <- 0
       temp3$delta_distance[temp3$Site==20] <- temp3$distance_m[temp3$Site==20] - temp3$distance_m[temp3$Site==100]
       temp3$delta_distance[temp3$Site==30] <- temp3$distance_m[temp3$Site==30] - temp3$distance_m[temp3$Site==200]
       temp3$delta_distance[temp3$Site==1] <-  temp3$distance_m[temp3$Site==1] -  temp3$distance_m[temp3$Site==20]
       temp3$delta_distance[temp3$Site==45] <- temp3$distance_m[temp3$Site==45] - mean(c(temp3$distance_m[temp3$Site==1], temp3$distance_m[temp3$Site==30]), na.rm = TRUE)
       temp3$delta_distance[temp3$Site==50] <- temp3$distance_m[temp3$Site==50] - temp3$distance_m[temp3$Site==45]
     }
     if(temp3$Reservoir=='FCR'){
       # calculate flow-weighting of inflows
       tot_flow <- temp3$Flow_cms[temp3$Site==99] + temp3$Flow_cms[temp3$Site==200] 
       prop_99 <- temp3$Flow_cms[temp3$Site==99]/tot_flow
       prop_200 <- temp3$Flow_cms[temp3$Site==200]/tot_flow
       
       temp3$delta[temp3$Site==99] <- 0
       temp3$delta[temp3$Site==200] <- 0
       temp3$delta[temp3$Site==20] <- temp3$value[temp3$Site==20] - (temp3$value[temp3$Site==99]*prop_99) - (temp3$value[temp3$Site==200]*prop_200)
       #temp3$delta[temp3$Site==20] <- temp3$value[temp3$Site==20] - (temp3$value[temp3$Site==99]) - (temp3$value[temp3$Site==200])
       #temp3$delta[temp3$Site==20] <- temp3$value[temp3$Site==20] - mean(c(temp3$value[temp3$Site==99], temp3$value[temp3$Site==200]), na.rm = TRUE)
       temp3$delta[temp3$Site==30] <- temp3$value[temp3$Site==30] - temp3$value[temp3$Site==20]
       temp3$delta[temp3$Site==45] <- temp3$value[temp3$Site==45] - temp3$value[temp3$Site==30]
       temp3$delta[temp3$Site==50] <- temp3$value[temp3$Site==50] - temp3$value[temp3$Site==45]
       temp3$delta_simple <- temp3$value[temp3$Site==50] - temp3$value[temp3$Site==99] - temp3$value[temp3$Site==200]
       temp3$delta_norm_simple <- temp3$delta_simple/642

       
       ## and calculate the delta distance
       temp3$delta_distance[temp3$Site==99] <- 0
       temp3$delta_distance[temp3$Site==200] <- 0
       temp3$delta_distance[temp3$Site==20] <- temp3$distance_m[temp3$Site==20] - (temp3$distance_m[temp3$Site==99] - temp3$distance_m[temp3$Site==200])
       temp3$delta_distance[temp3$Site==30] <- temp3$distance_m[temp3$Site==30] - temp3$distance_m[temp3$Site==20]
       temp3$delta_distance[temp3$Site==45] <- temp3$distance_m[temp3$Site==45] - temp3$distance_m[temp3$Site==30]
       temp3$delta_distance[temp3$Site==50] <- temp3$distance_m[temp3$Site==50] - temp3$distance_m[temp3$Site==45]

     }  
    test <- rbind(test, temp3)   
   }
  }
}

test <- test[-1,]

### delta = what was produced between the two sites, so a positive value indicates that nutrients were produced, 
### negative means they were buried

vars_keep <- c('A', 'T', 'Chla_ugL', 'DOC_mgL', 'NH4_ugL', 'NO3NO2_ugL', 'SRP_ugL', 'TN_ugL', 'TP_ugL', 'Sp_cond_uScm')
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
sp <- test[test$distance_from_stream > 0 & test$variable=='Sp_cond_uScm',]
#hist(sp$value)

for(i in 1:nrow(test)){
  if(test$variable[i]=='Sp_cond_uScm'){
    test$loq[i] <- 0.01*test$value[i] + 0.05
    test$delta_simple[i] <- NA
    
  }
}


# set order of variables
levels <- c('T', 'A', 'DOC_mgL',  
            'NH4_ugL', 'NO3NO2_ugL', 'SRP_ugL',
            'Chla_ugL','TN_ugL', 'TP_ugL',
            'Sp_cond_uScm')
#'TN_TP', 'DP_TP', 'DN_TN') #'HIX', 'BIX'
labels <- c('T-autoch',  'A-alloch', 'DOC',
            'NH4', 'NO3', 'SRP',
            'Chl-a', 'TN', 'TP',
            'Sp Cond') 
names(labels) <- levels
test$variable <- factor(test$variable, levels = levels)


b <- ggplot(data = test[test$distance_from_stream >0 & test$Reservoir=='BVR',], aes(x = distance_m, y = delta)) +
  xlim(325, 1200) +
  facet_wrap(~variable, scales = 'free_y', ncol = 3) +
  geom_point(aes(col = as.factor(month(Date))), size = 2) +
  geom_line(aes(col = as.factor(month(Date)))) +
  geom_hline(aes(yintercept = 0)) +
  geom_point(aes(x = 1200, y = delta_simple, col = as.factor(month(Date))), size = 4)+
  geom_ribbon(aes(ymin = delta -loq, ymax = delta + loq, fill = as.factor(month(Date)), col = as.factor(month(Date))), alpha = 0.1, linetype = 0)+
  scale_color_manual(values = rev(hcl.colors(7, "Zissou 1"))) +
  scale_fill_manual(values = rev(hcl.colors(7, "Zissou 1"))) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  ylab('Delta from upstream (concentration)') +
  xlab('Distance from upstream (m)') +
  ggtitle('Beaverdam Reservoir')
b


f <- ggplot(data = test[test$distance_from_stream >0 & test$Reservoir=='FCR',], 
            aes(x = distance_m, y = delta)) +
  xlim(95, 710) +
  facet_wrap(~variable, scales = 'free_y', ncol = 3) +
  geom_line(aes(col = as.factor(month(Date)))) +
  geom_point(aes(col = as.factor(month(Date))), size = 2) +
  geom_hline(aes(yintercept = 0)) +
  geom_point(aes(x = 710, y = delta_simple, col = as.factor(month(Date))), size = 4)+
  geom_ribbon(aes(ymin = delta -loq, ymax = delta + loq, fill = as.factor(month(Date)), col = as.factor(month(Date))), alpha = 0.1, linetype = 0)+
  scale_color_manual(values = rev(hcl.colors(7, "Zissou 1"))) +
  scale_fill_manual(values = rev(hcl.colors(7, "Zissou 1"))) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  ylab('Delta from upstream (concentration)') +
  xlab('Distance from upstream (m)') +
  ggtitle('Falling Creek Reservoir')
f

ggarrange(b, f, nrow = 1, ncol = 2, common.legend = TRUE, legend = 'right') 

#########################################################################################################
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
        temp3$delta_load_simple <- temp3$load[temp3$Site==50] - sum(temp3$load[temp3$Site==100], temp3$load[temp3$Site==200], na.rm = TRUE)
        
      }
      if(temp3$Reservoir=='FCR'){
        temp3$delta_load_spatial[temp3$Site==99] <- 0
        temp3$delta_load_spatial[temp3$Site==200] <- 0
        temp3$delta_load_spatial[temp3$Site==20] <- temp3$load[temp3$Site==20] - sum(temp3$load[temp3$Site==99], temp3$load[temp3$Site==200], na.rm = TRUE)
        temp3$delta_load_spatial[temp3$Site==30] <- temp3$load[temp3$Site==30] - temp3$load[temp3$Site==20]
        temp3$delta_load_spatial[temp3$Site==45] <- temp3$load[temp3$Site==45] - temp3$load[temp3$Site==30]
        temp3$delta_load_spatial[temp3$Site==50] <- temp3$load[temp3$Site==50] - temp3$load[temp3$Site==45]
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
sp <- test_load[test_load$distance_from_stream > 0 & test_load$variable=='Sp_cond_uScm',]
hist(sp$value)

for(i in 1:nrow(test_load)){
  if(test_load$variable[i]=='Sp_cond_uScm'){
    test_load$loq[i] <- 0.01*test_load$value[i] + 0.05
  }
}

test_load$loq_load <- test_load$loq*test_load$Flow_cms
vars_process <- c('T', 'A', 'DOC_mgL', 
                  'NH4_ugL', 'NO3NO2_ugL', 'SRP_ugL',
                  'Chla_ugL', 'TN_ugL', 'TP_ugL',
                   'Sp_cond_uScm')
test_load <- test_load[test_load$variable %in% vars_process,]

## set the order of hte plots
levels <- c('T', 'A', 'DOC_mgL', 
                  'NH4_ugL', 'NO3NO2_ugL', 'SRP_ugL',
                  'Chla_ugL', 'TN_ugL', 'TP_ugL',
                  'Sp_cond_uScm')
labels <- c('T-autoch',  'A-alloch', 'DOC',
            'NH4', 'NO3', 'SRP',
            'Chl-a', 'TN', 'TP',
            "Sp Cond")
names(labels) <- levels
test_load$variable <- factor(test_load$variable, levels = levels)




bl <- ggplot(data = test_load[test_load$distance_from_stream >0 & test_load$Reservoir=='BVR',], aes(x = distance_m, y = delta_load_spatial)) +
  facet_wrap(~variable, scales = 'free_y', ncol = 3) +
  geom_line(aes(col = as.factor(month(Date)))) +
  geom_hline(aes(yintercept = 0)) +
  geom_point(aes(col = as.factor(month(Date))), size = 2) +
  geom_point(aes(x = 1200, y = delta_load_simple, col = as.factor(month(Date))), size = 4)+
  #geom_ribbon(aes(ymin = delta_load_spatial -loq_load, ymax = delta_load_spatial + loq_load, 
  #                col = as.factor(month(Date)), fill = as.factor(month(Date))), 
  #            alpha = 0.1, linetype = 0)+
  scale_color_manual(values = rev(hcl.colors(7, "Zissou 1"))) +
  scale_fill_manual(values = rev(hcl.colors(7, "Zissou 1"))) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  ylab('Delta load from upstream (concentration*discharge)') +
  xlab('Distance from upstream (m)') +
  ggtitle('Beaverdam Reservoir')
bl

fl <- ggplot(data = test_load[test_load$distance_from_stream >0 & test_load$Reservoir=='FCR',], aes(x = distance_m, y = delta_load_spatial)) +
  facet_wrap(~variable, scales = 'free_y', ncol = 3) +
  geom_line(aes(col = as.factor(month(Date)))) +
  geom_hline(aes(yintercept = 0)) +
  geom_point(aes(col = as.factor(month(Date))), size = 2) +
  geom_point(aes(x = 710, y = delta_load_simple, col = as.factor(month(Date))), size = 4)+
  #  geom_ribbon(aes(ymin = delta_load_spatial -loq_load, ymax = delta_load_spatial + loq_load, 
#                  col = as.factor(month(Date)), fill = as.factor(month(Date))), alpha = 0.1, linetype = 0)+
  scale_color_manual(values = rev(hcl.colors(7, "Zissou 1"))) +
  scale_fill_manual(values = rev(hcl.colors(7, "Zissou 1"))) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  ylab('Delta load from upstream (concentration*discharge)') +
  xlab('Distance from upstream (m)') +
  ggtitle('Falling Creek Reservoir')
fl

ggarrange(bl, fl, nrow = 1, ncol = 2, common.legend = TRUE, legend = 'right') 


##################################################################################################################
########################## and the ug/day calculations
# convert delta_load_* from ug/L*m3/s to ug/day: *1000 to go from L -> m3 and *86400 to go from sec -> day
test_load$delta_load_spatial_ugday <- test_load$delta_load_spatial*1000*86400
test_load$delta_load_simple_ugday <- test_load$delta_load_simple*1000*86400


bl <- ggplot(data = test_load[test_load$distance_from_stream >0 & test_load$Reservoir=='BVR',], 
             aes(x = distance_m, y = delta_load_spatial_ugday)) +
  facet_wrap(~variable, scales = 'free_y', ncol = 3) +
  geom_line(aes(col = as.factor(month(Date)))) +
  geom_hline(aes(yintercept = 0)) +
  geom_point(aes(col = as.factor(month(Date))), size = 2) +
  geom_point(aes(x = 1200, y = delta_load_simple_ugday, col = as.factor(month(Date))), size = 4)+
  #geom_ribbon(aes(ymin = delta_load_spatial -loq_load, ymax = delta_load_spatial + loq_load, 
  #                col = as.factor(month(Date)), fill = as.factor(month(Date))), 
  #            alpha = 0.1, linetype = 0)+
  scale_color_manual(values = rev(hcl.colors(7, "Zissou 1"))) +
  scale_fill_manual(values = rev(hcl.colors(7, "Zissou 1"))) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  ylab('Delta load from upstream (ug/day)') +
  xlab('Distance from upstream (m)') +
  ggtitle('Beaverdam Reservoir')
bl

fl <- ggplot(data = test_load[test_load$distance_from_stream >0 & test_load$Reservoir=='FCR',], 
             aes(x = distance_m, y = delta_load_spatial_ugday)) +
  facet_wrap(~variable, scales = 'free_y', ncol = 3) +
  geom_line(aes(col = as.factor(month(Date)))) +
  geom_hline(aes(yintercept = 0)) +
  geom_point(aes(col = as.factor(month(Date))), size = 2) +
  geom_point(aes(x = 710, y = delta_load_simple_ugday, col = as.factor(month(Date))), size = 4)+
  #  geom_ribbon(aes(ymin = delta_load_spatial -loq_load, ymax = delta_load_spatial + loq_load, 
  #                  col = as.factor(month(Date)), fill = as.factor(month(Date))), alpha = 0.1, linetype = 0)+
  scale_color_manual(values = rev(hcl.colors(7, "Zissou 1"))) +
  scale_fill_manual(values = rev(hcl.colors(7, "Zissou 1"))) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  ylab('Delta load from upstream (ug/day)') +
  xlab('Distance from upstream (m)') +
  ggtitle('Falling Creek Reservoir')

ggarrange(bl, fl, nrow = 1, ncol = 2, common.legend = TRUE, legend = 'right') 


##################################################################################################################
########################## take mass/day and normalize by specific conductance

# go through loop again and divided by sp cond 
#/temp3$value[temp3$Site==20 & temp3$variable=='Sp_cond_uScm']

test_load <- NA

for(j in 1:length(vars)){
  temp <- long_load_2[long_load_2$variable==vars[j]|long_load_2$variable=='Sp_cond_uScm',]
  for (k in 1:length(dates)) {
    temp2 <- temp[temp$Date==dates[k],]
    for(m in 1:length(res)){
      temp3 <- temp2[temp2$Reservoir==res[m],]
      if(temp3$Reservoir=='BVR'){
        temp3$delta_load_spatial[temp3$Site==100] <- 0
        temp3$delta_load_spatial[temp3$Site==200] <- 0
        temp3$delta_load_spatial[temp3$Site==20] <- temp3$load[temp3$Site==20 & temp3$variable==vars[j]]/temp3$value[temp3$Site==20 & temp3$variable=='Sp_cond_uScm'] - temp3$load[temp3$Site==100 & temp3$variable==vars[j]]/temp3$value[temp3$Site==100 & temp3$variable=='Sp_cond_uScm']
        temp3$delta_load_spatial[temp3$Site==30] <- temp3$load[temp3$Site==30 & temp3$variable==vars[j]]/temp3$value[temp3$Site==30 & temp3$variable=='Sp_cond_uScm'] - temp3$load[temp3$Site==200 & temp3$variable==vars[j]]/temp3$value[temp3$Site==200 & temp3$variable=='Sp_cond_uScm']
        temp3$delta_load_spatial[temp3$Site==1] <- temp3$load[temp3$Site==1 & temp3$variable==vars[j]]/temp3$value[temp3$Site==1 & temp3$variable=='Sp_cond_uScm'] - temp3$load[temp3$Site==20 & temp3$variable==vars[j]]/temp3$value[temp3$Site==20 & temp3$variable=='Sp_cond_uScm']
        temp3$delta_load_spatial[temp3$Site==45] <- temp3$load[temp3$Site==45 & temp3$variable==vars[j]]/temp3$value[temp3$Site==45 & temp3$variable=='Sp_cond_uScm'] - mean(c(temp3$load[temp3$Site==1 & temp3$variable==vars[j]]/temp3$value[temp3$Site==1 & temp3$variable=='Sp_cond_uScm'], temp3$load[temp3$Site==30 & temp3$variable==vars[j]]/temp3$value[temp3$Site==30 & temp3$variable=='Sp_cond_uScm']), na.rm = TRUE)
        #temp3$delta_load_spatial[temp3$Site==45] <- temp3$load[temp3$Site==45] - (temp3$load[temp3$Site==1] - temp3$load[temp3$Site==30])
        temp3$delta_load_spatial[temp3$Site==50] <- temp3$load[temp3$Site==50 & temp3$variable==vars[j]]/temp3$value[temp3$Site==50 & temp3$variable=='Sp_cond_uScm'] - temp3$load[temp3$Site==45 & temp3$variable==vars[j]]/temp3$value[temp3$Site==45 & temp3$variable=='Sp_cond_uScm']
        temp3$delta_load_simple <- temp3$load[temp3$Site==50 & temp3$variable==vars[j]]/temp3$value[temp3$Site==50 & temp3$variable=='Sp_cond_uScm'] - sum(temp3$load[temp3$Site==100& temp3$variable==vars[j]]/temp3$value[temp3$Site==100 & temp3$variable=='Sp_cond_uScm'], temp3$load[temp3$Site==200 & temp3$variable==vars[j]]/temp3$value[temp3$Site==200 & temp3$variable=='Sp_cond_uScm'], na.rm = TRUE)
        
      }
      if(temp3$Reservoir=='FCR'){
        temp3$delta_load_spatial[temp3$Site==99] <- 0
        temp3$delta_load_spatial[temp3$Site==200] <- 0
        temp3$delta_load_spatial[temp3$Site==20] <- temp3$load[temp3$Site==20 & temp3$variable==vars[j]]/temp3$value[temp3$Site==20 & temp3$variable=='Sp_cond_uScm'] - sum(temp3$load[temp3$Site==99 & temp3$variable==vars[j]]/temp3$value[temp3$Site==99 & temp3$variable=='Sp_cond_uScm'], temp3$load[temp3$Site==200 & temp3$variable==vars[j]]/temp3$value[temp3$Site==200 & temp3$variable=='Sp_cond_uScm'], na.rm = TRUE)
        temp3$delta_load_spatial[temp3$Site==30] <- temp3$load[temp3$Site==30 & temp3$variable==vars[j]]/temp3$value[temp3$Site==30 & temp3$variable=='Sp_cond_uScm'] - temp3$load[temp3$Site==20 & temp3$variable==vars[j]]/temp3$value[temp3$Site==20 & temp3$variable=='Sp_cond_uScm']
        temp3$delta_load_spatial[temp3$Site==45] <- temp3$load[temp3$Site==45 & temp3$variable==vars[j]]/temp3$value[temp3$Site==45 & temp3$variable=='Sp_cond_uScm'] - temp3$load[temp3$Site==30 & temp3$variable==vars[j]]/temp3$value[temp3$Site==30 & temp3$variable=='Sp_cond_uScm']
        temp3$delta_load_spatial[temp3$Site==50] <- temp3$load[temp3$Site==50 & temp3$variable==vars[j]]/temp3$value[temp3$Site==50 & temp3$variable=='Sp_cond_uScm'] - temp3$load[temp3$Site==45 & temp3$variable==vars[j]]/temp3$value[temp3$Site==45 & temp3$variable=='Sp_cond_uScm']
        temp3$delta_load_simple <- temp3$load[temp3$Site==50 & temp3$variable==vars[j]]/temp3$value[temp3$Site==50 & temp3$variable=='Sp_cond_uScm'] - sum(temp3$load[temp3$Site==99 & temp3$variable==vars[j]]/temp3$value[temp3$Site==99 & temp3$variable=='Sp_cond_uScm'], temp3$load[temp3$Site==200 & temp3$variable==vars[j]]/temp3$value[temp3$Site==200 & temp3$variable=='Sp_cond_uScm'], na.rm = TRUE)
        
        
      }  
      test_load <- rbind(test_load, as.data.frame(temp3))   
    }
  }
}

test_load <- test_load[-1,]
exclude <- c('FCR_102', 'FCR_101', 'FCR_100', 'FCR_1')
test_load <- test_load[!(test_load$site_res %in% exclude),]

vars_process <- c('T', 'A', 'DOC_mgL', 
                  'NH4_ugL', 'NO3NO2_ugL', 'SRP_ugL',
                  'Chla_ugL', 'TN_ugL', 'TP_ugL',
                  'Sp_cond_uScm')
test_load <- test_load[test_load$variable %in% vars_process,]

## set the order of hte plots
levels <- c('NH4_ugL', 'NO3NO2_ugL', 'SRP_ugL',
            'DOC_mgL', 'TN_ugL', 'TP_ugL',
            'Chla_ugL','T', 'A',  
            'Sp_cond_uScm')
labels_f <- c('d) NH4 (μg/L)', 'e) NO3 (μg/L)', 'f) SRP (μg/L)',
              'j) DOC (mg/L)', 'k) TN (μg/L)', 'l) TP (μg/L)',
              'p) Chl-a (μg/L)', 'q) T-autoch (RFU)',  'r) A-alloch (RFU)', 'Sp Cond') 
names(labels_f) <- levels
test_load$variable <- factor(test_load$variable, levels = levels)

f_sp <- ggplot(data = test_load[test_load$distance_from_stream >0 & test_load$variable!='Sp_cond_uScm' & test_load$Reservoir=='FCR',], 
               aes(x = distance_m, y = delta_load_spatial)) +
  facet_wrap(~variable, scales = 'free_y', ncol = 3,  labeller = labeller(variable = labels_f)) +
  geom_line(aes(col = as.factor(month(Date)))) +
  geom_hline(aes(yintercept = 0)) +
  geom_point(aes(col = as.factor(month(Date))), size = 2) +
  geom_point(aes(x = 710, y = delta_load_simple, col = as.factor(month(Date))), size = 4)+
  #  geom_ribbon(aes(ymin = delta_load_spatial -loq_load, ymax = delta_load_spatial + loq_load, 
  #                  col = as.factor(month(Date)), fill = as.factor(month(Date))), alpha = 0.1, linetype = 0)+
  scale_color_manual(values = rev(hcl.colors(7, "Zissou 1")), name = 'Month') +
  scale_fill_manual(values = rev(hcl.colors(7, "Zissou 1"))) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  ylab('Delta mass/sp cond (µg m3 cm / L s µs)') +
  xlab('Distance from upstream (m)') +
  ggtitle('Falling Creek Reservoir')
f_sp

labels_b <- c('a) NH4 (μg/L)', 'b) NO3 (μg/L)', 'c) SRP (μg/L)',
              'g) DOC (mg/L)', 'h) TN (μg/L)', 'i) TP (μg/L)',
              'm) Chl-a (μg/L)', 'n) T-autoch (RFU)',  'o) A-alloch (RFU)', 'sp cond') 
names(labels_b) <- levels
test_load$variable <- factor(test_load$variable, levels = levels)

b_sp <- ggplot(data = test_load[test_load$distance_from_stream >0 & test_load$variable!='Sp_cond_uScm'  & test_load$Reservoir=='BVR',], 
             aes(x = distance_m, y = delta_load_spatial)) +
  facet_wrap(~variable, scales = 'free_y', ncol = 3,  labeller = labeller(variable = labels_b)) +
  geom_line(aes(col = as.factor(month(Date)))) +
  geom_hline(aes(yintercept = 0)) +
  geom_point(aes(col = as.factor(month(Date))), size = 2) +
  geom_point(aes(x = 1200, y = delta_load_simple, col = as.factor(month(Date))), size = 4)+
  #geom_ribbon(aes(ymin = delta_load_spatial -loq_load, ymax = delta_load_spatial + loq_load, 
  #                col = as.factor(month(Date)), fill = as.factor(month(Date))), 
  #            alpha = 0.1, linetype = 0)+
  scale_color_manual(values = rev(hcl.colors(7, "Zissou 1")), name = 'Month') +
  scale_fill_manual(values = rev(hcl.colors(7, "Zissou 1"))) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  ylab('Delta mass/sp cond (µg m3 cm / L s µs)') +
  xlab('Distance from upstream (m)') +
  ggtitle('Beaverdam Reservoir')
b_sp


ggarrange(b_sp, f_sp, nrow = 1, ncol = 2, common.legend = TRUE, legend = 'right') 

#ggplot(data = test_load[test_load$variable=='Sp_cond_uScm' & test_load$Reservoir=='FCR',], aes(x = Date, y = value)) + geom_point(aes(color = as.factor(Site)))


##### calculate normalized distance 
test <- test %>% 
  mutate(delta_norm = (delta/delta_distance)) %>%
  mutate(norm_distance = ifelse(Reservoir=='BVR', distance_m/1117, distance_m/642))


ggplot(data = test[test$distance_from_stream>0,], aes(x = distance_m, y = delta_norm)) +
  facet_wrap(~variable, scales = 'free_y', ncol = 3) +
  geom_line(aes(col = as.factor(month(Date)), linetype = Reservoir)) +
  geom_hline(aes(yintercept = 0)) +
  scale_color_manual(values = rev(hcl.colors(7, "Zissou 1"))) +
  #geom_point(aes(x = 600, y = delta_simple, col = as.factor(month(Date))), size = 4) +
  ylab('Delta concentration over delta distance') +
  xlab('Distance from upstream (m)') +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

#ggplot(data = test[test$distance_from_stream>0,], aes(x = norm_distance, y = delta_norm)) +
#  facet_wrap(~variable, scales = 'free') +
#  scale_color_manual(values = rev(hcl.colors(7, "Zissou 1"))) +
#  geom_line(aes(col = as.factor(month(Date)), linetype = Reservoir)) +
#  geom_hline(aes(yintercept = 0)) +
#  xlab('Normalized distance (% of total reservoir length)') +
#  ylab('Delta concentration over delta distance') 

bd <- ggplot(data = test[test$distance_from_stream>0 & test$Reservoir=='BVR',], aes(x = distance_m, y = delta_norm)) +
  facet_wrap(~variable, scales = 'free_y', ncol = 3) +
  geom_point(aes(col = as.factor(month(Date))), size = 2) +
  geom_point(aes(x = 1200, y = delta_norm_simple, col = as.factor(month(Date))), size = 4)+
  scale_color_manual(values = rev(hcl.colors(7, "Zissou 1"))) +
  geom_line(aes(col = as.factor(month(Date)))) +
  geom_hline(aes(yintercept = 0)) +
  ylab('Delta concentration over delta distance') +
  xlab('Distance from upstream (m)') +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'none') +
  ggtitle('Beaverdam Reservoir')



fd <- ggplot(data = test[test$distance_from_stream>0 & test$Reservoir=='FCR',], aes(x = distance_m, y = delta_norm)) +
  facet_wrap(~variable, scales = 'free_y', ncol = 3) +
  scale_color_manual(values = rev(hcl.colors(7, "Zissou 1"))) +
  geom_point(aes(col = as.factor(month(Date))), size = 2) +
  geom_point(aes(x = 710, y = delta_norm_simple, col = as.factor(month(Date))), size = 4)+
  geom_line(aes(col = as.factor(month(Date)))) +
  geom_hline(aes(yintercept = 0)) +
  ylab('Delta concentration over delta distance') +
  xlab('Distance from upstream (m)') +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  ggtitle('Falling Creek Reservoir')

ggarrange(bd, fd, nrow = 1, ncol = 2, common.legend = TRUE, legend = 'right') 





#################################################################################################################
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
