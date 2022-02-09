### Script to calculate WRT for FCR and BVR on RC days
### 13 Oct 2020, A Hounshell


# Load libraries
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(plotrix)

# Load in inflow data
inflow <- read.csv("./Data/raw_data/flow_edi.csv")

# Separate by site
fcr_99 <- inflow %>% filter(Reservoir=="FCR" & Site=="99") %>% select(Date,Site,Flow_cms)
fcr_200 <- inflow %>% filter(Reservoir=="FCR" & Site=="200") %>% select(Date,Site,Flow_cms)
fcr_1 <- inflow %>% filter(Reservoir=="FCR" & Site=="1") %>% select(Date,Site,Flow_cms)
bvr_100 <- inflow %>% filter(Reservoir=="BVR"& Site=="100") %>% select(Date,Site,Flow_cms)
bvr_200 <- inflow %>% filter(Reservoir=="BVR" & Site=="200") %>% select(Date,Site,Flow_cms)

# Combine by reservoir
fcr <- rbind.data.frame(fcr_99,fcr_200)
fcr <- fcr %>% group_by(Date) %>% summarise_all(funs(sum)) %>% filter(Site=="299")

bvr <- rbind.data.frame(bvr_100,bvr_200)
bvr <- bvr %>% group_by(Date) %>% summarise_all(funs(sum)) %>% filter(Site=="300")

# Calculate WRT
# FCR: Assume full pond volume (3.1E5 m3)
fcr <- fcr %>% mutate(wrt_d = (3.1E5/Flow_cms)*(1/60)*(1/60)*(1/24))
mean(fcr$wrt_d)
sd(fcr$wrt_d)
std.error(fcr$wrt_d)

# BVR: Averaged water level for 2019 (10.91m) which corresponds to 8.9E5 m3
# maybe need to use variable volume? water level data here: https://docs.google.com/spreadsheets/d/1XLJu5T7qsWtbQDBDEv3_63yfNB_GVXgs/edit#gid=353013140
bvr <- bvr %>% mutate(wrt_d = (8.9E5/Flow_cms)*(1/60)*(1/60)*(1/24))
mean(bvr$wrt_d)
sd(bvr$wrt_d)
std.error(bvr$wrt_d)

# Plot
ggplot()+
  geom_line(bvr,mapping=aes(x=Date,y=wrt_d,color='BVR',group=1),size=1)+
  geom_point(bvr,mapping=aes(x=Date,y=wrt_d,color='BVR'),size=2)+
  geom_line(fcr,mapping=aes(x=Date,y=wrt_d,color='FCR',group=1),size=1)+
  geom_point(fcr,mapping=aes(x=Date,y=wrt_d,color='FCR'),size=2)+
  theme_classic(base_size=15)

write.csv(wrt,"./Data/RC_WRT.csv")

