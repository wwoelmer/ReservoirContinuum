# make spearman correlation matrix for each reservoir

library(Hmisc)
library(tidyverse)
library(corrplot)

data <-read.csv('./Data/continuum_data.csv')
data$Date <- as.Date(data$DateTime)
data$connectivity <- as.factor(data$connectivity)

data <- data %>% dplyr::select(Date, everything(),-DateTime, -Depth_m, -(DIC_mgL:Flag_DN))
data <- data %>% mutate(TN_TP = TN_ugL/TP_ugL) %>% 
  mutate(DP_TP = SRP_ugL/TP_ugL) %>% 
  mutate(DN_TN = (NH4_ugL + NO3NO2_ugL)/TN_ugL)

data$Site <- as.factor(data$Site)

long <- data %>%   
  dplyr::select(Site, Reservoir, Date, TN_ugL:Chla_ugL, A:BIX, TN_TP:DN_TN) %>% 
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
    if(res[j]=='FCR'){
      temp2 <- temp2[,-c(4,9,12, 13)]
    }
    temp2 <- temp2[,-c(1,2,3)]
    cor <- rcorr(as.matrix(temp2), type = 'spearman')
  #  for(k in 1:nrow(cor$r)){
  #    for(l in 1:ncol(cor$r)){
  #    if(!is.na(cor$P[k,l]) & cor$P[k,l] > 0.05){
  #      cor$r[k,l] <- NA
   #   }
  #  }
 # }
    print(c(vars[i], res[j]))
    print(cor$r)
    
      print(corrplot(cor$r, method = 'color', 
                   diag = FALSE, 
                   p.mat = cor$P, 
                   insig = 'blank',  
                   pch.cex = 1, 
                   pch.col = "white",
                   addCoef.col = "white", bg = "black", 
                   tl.col="black",  tl.offset=0.5,
                   type = 'lower', 
                   title = paste0(vars[i], ' ', res[j]),
                   mar=c(0,0,1,0)))
    
    
    #write.csv(cor$r, paste0('./Data/correlation_matrices/', vars[i], '_', res[j], 'spearman.csv'), row.names = FALSE)
  }
}

# this produces 28 different files though so how can I streamline this process nad pick out the highest correlations? or only significant correlations

install.packages('ggcorrplot')
library(ggcorrplot)
install.packages('corrplot')
ggcorrplot(round(as.matrix(cor$r), 1))
corrplot(cor$r, method = 'color', p.mat = cor$P, insig = 'blank', type = 'lower', title = paste0(vars[i], ' ', res[j]),
         mar=c(0,0,1,0))
corrplot(cor$r, method = 'color', p.mat = cor$P)

