# make spearman correlation matrix for each reservoir

library(Hmisc)
library(tidyverse)

data <-read.csv('./Data/continuum_ww.csv')
data$Date <- as.Date(data$DateTime)
data$connectivity <- as.factor(data$connectivity)

data <- data %>% select(Date, everything(),-DateTime, -Depth_m, -(DIC_mgL:Flag_DN))
data <- data %>% mutate(TN_TP = TN_ugL/TP_ugL) %>% 
  mutate(DP_TP = SRP_ugL/TP_ugL) %>% 
  mutate(DN_TN = (NH4_ugL + NO3NO2_ugL)/TN_ugL)

long <- data %>%   
  select(Site, Reservoir, Date, TN_ugL:Chla_ugL, A:BIX, TN_TP:DN_TN) %>% 
  pivot_longer(TN_ugL:DN_TN, names_to = 'variable', values_to = 'value')

site_wide <- long %>% 
  pivot_wider(names_from = Site, values_from = value)

vars <- unique(site_wide$variable)
res <- unique(site_wide$Reservoir)

for(i in 1:length(vars)){
  temp <- site_wide[site_wide$variable==vars[i],]
  for(j in 1:length(res)){
    temp2 <- temp[temp$Reservoir==res[j],]
    if(res[j]=='BVR'){
      temp2 <- temp2[,-c(11, 12, 13)]
    }
    temp2 <- temp2[,-c(1,2,3)]
    cor <- rcorr(as.matrix(temp2), type = 'spearman')
    for(k in 1:nrow(cor$r)){
      for(l in 1:ncol(cor$r)){
      if(!is.na(cor$P[k,l]) & cor$P[k,l] > 0.05){
        cor$r[k,l] <- NA
      }
    }
  }
    print(c(vars[i], res[j]))
    print(cor$r)
    #write.csv(cor$r, paste0('./Data/correlation_matrices/', vars[i], '_', res[j], 'spearman.csv'), row.names = FALSE)
  }
}

# this produces 28 different files though so how can I streamline this process nad pick out the highest correlations? or only significant correlations