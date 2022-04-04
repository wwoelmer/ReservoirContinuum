
library(Hmisc)
library(tidyverse)
library(corrplot)

data <-read.csv('./Data/continuum_data.csv')

# correlation between peak t and chl-a
data <- data %>% 
  select(Date, res_site, Reservoir, Site, Chla_ugL, T)

cor_data <- data %>% 
  select(Chla_ugL, T)

# for all BVR and FCR sites
rcorr(as.matrix(cor_data), type = 'spearman')

# what about grouping by reservoir
res <- unique(data$Reservoir)

for(i in 1:length(res)){
  temp <- data[data$Reservoir==res[i],]
  cor_data <- temp %>% 
    select(Chla_ugL, T)
  print(rcorr(as.matrix(cor_data), type = 'spearman'))
  print(res[i])
  
}

