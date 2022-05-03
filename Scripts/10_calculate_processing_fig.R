
library(tidyverse)
library(patchwork)
library(lubridate)
library(EnvStats)
library(ggpubr)
#library(ggpmisc)

r_col <- c('olivedrab3', 'royalblue1')

data <-read.csv('./Data/continuum_data.csv')
data$Date <- as.Date(data$Date)

# some SRP values are 0, so add very very small value to these for dividing by this value later
min_val <- min(data[data$SRP_ugL>0,"SRP_ugL"], na.rm = TRUE)

data <- data %>% 
  mutate(SRP_ugL = SRP_ugL + min_val)  
  

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


##################################################################################################################
########################## take mass/day and normalize by specific conductance
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

# assign flows for all BVR sites based on B50
for(i in 1:length(unique(long_load$Date))){
  if(length(long_load$Flow_cms[long_load$site_res=='BVR_50'& long_load$Date==unique(long_load$Date)[i]]) > 1){
    long_load$Flow_cms[long_load$site_res %in% bvr_res & long_load$Date==unique(long_load$Date)[i]] <- long_load$Flow_cms[long_load$site_res=='BVR_50'& long_load$Date==unique(long_load$Date)[i]]
  }
}

# calculate loads for each day 
long_load <- long_load %>% 
  group_by(Date, Reservoir, Site) %>% 
  mutate(load = value*Flow_cms)


# use loads to calculate the delta concentration instead of raw values
long_load$delta_load_simple <- NA
long_load$delta_load_spatial <- NA
long_load_2 <- long_load


# go through loop and divided by sp cond 

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
levels <- c('TN_ugL', 'TP_ugL','Chla_ugL',
            'NH4_ugL', 'NO3NO2_ugL', 'SRP_ugL',
            'DOC_mgL', 'T', 'A',  
            'Sp_cond_uScm')

labels_f <- c('d) TN (μg/L)', 'e) TP (μg/L)', 'f) Chl-a (μg/L)',
              'j) NH4 (μg/L)', 'k) NO3 (μg/L)', 'l) SRP (μg/L)',
              'p) DOC (mg/L)', 'q) T-autoch (RFU)',  'r) A-alloch (RFU)', 'Sp Cond') 
names(labels_f) <- levels
test_load$variable <- factor(test_load$variable, levels = levels)

f_sp <- ggplot(data = test_load[test_load$distance_from_stream >0 & test_load$variable!='Sp_cond_uScm' & test_load$Reservoir=='FCR',], 
               aes(x = distance_m, y = delta_load_spatial)) +
  facet_wrap(~variable, scales = 'free_y', ncol = 3,  labeller = labeller(variable = labels_f)) +
  geom_line(aes(col = as.factor(month(Date)))) +
  #geom_line(col = 'white') +
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

labels_b <- c('a) TN (μg/L)', 'b) TP (μg/L)', 'c) Chl-a (μg/L)',
              'g) NH4 (μg/L)', 'h) NO3 (μg/L)', 'i) SRP (μg/L)',
              'm) DOC (mg/L)', 'n) T-autoch (RFU)',  'o) A-alloch (RFU)', 'sp cond') 
names(labels_b) <- levels
test_load$variable <- factor(test_load$variable, levels = levels)

b_sp <- ggplot(data = test_load[test_load$distance_from_stream >0 & test_load$variable!='Sp_cond_uScm'  & test_load$Reservoir=='BVR',], 
               aes(x = distance_m, y = delta_load_spatial)) +
  facet_wrap(~variable, scales = 'free_y', ncol = 3,  labeller = labeller(variable = labels_b)) +
  geom_line(aes(col = as.factor(month(Date)))) +
  #geom_line(col = 'white') +
  geom_hline(aes(yintercept = 0)) +
  geom_point(aes(col = as.factor(month(Date))), size = 2) +
  geom_point(aes(x = 1200, y = delta_load_simple, col = as.factor(month(Date, label = TRUE, abbr = TRUE))), size = 4)+
  scale_color_manual(values = rev(hcl.colors(7, "Zissou 1")), name = 'Month') +
  scale_fill_manual(values = rev(hcl.colors(7, "Zissou 1"))) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  ylab('Delta mass/sp cond (µg m3 cm / L s µs)') +
  xlab('Distance from upstream (m)') +
  ggtitle('Beaverdam Reservoir')
b_sp


process <- ggarrange(b_sp, f_sp, nrow = 1, ncol = 2, common.legend = TRUE, legend = 'right') 
process
ggsave('./Figures/Fig6_processing.png', process)

##########################################################################################################################################

