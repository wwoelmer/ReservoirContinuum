# calculate outflow estimates for Beaverdam Reservoir
# pull together inflow estimates calculating using Thornwaite Mather Water Balance model for different project
# more about model and code here: https://github.com/CareyLabVT/BVR-GLM/blob/master/Scripts_Data/BVR_TMWB_flow.R
# and combine with observed water levels at BVR which are used to calculate volume
# and estimate outflow as the balance between these two

library(tidyverse)
library(lubridate)


# First, read in inflow file generated from Thronthwaite Overland flow model + groundwater recharge
# From HW: for entire watershed; units in m3/s
# inflow model using NLDAS precip and temp data
inflow <- read_csv("./Data/raw_data/bvr_flow_calcs_git.csv")
inflow$time = as.POSIXct(strptime(inflow$time,"%Y-%m-%d", tz="EST"))
inflow <- inflow[,-c(1)]
names(inflow)[2] <- "FLOW"
inflow$FLOW = inflow$FLOW/86400
inflow <- inflow %>% 
  filter(time >= "2014-01-01")

#diagnostic plot
plot(inflow$time, inflow$FLOW)


# Load in water level + volume data for BVR
# Calculated using WVWA + Carey Lab BVR water level observations and joined DEM + 2018 Bathymetry survey
# See: BVR_Volume script for Matlab
vol <- read.csv("./Data/raw_data/bvr_vol_git.csv")
vol$Date <- as.POSIXct(strptime(vol$Date, "%m/%d/%Y", tz = "EST"))

vol1 <- vol %>% filter(Date>=as.Date('2013-12-31')&Date<=as.Date('2019-12-30')) %>% select(Date,BVR_Vol_m3)
vol2 <- vol %>% filter(Date>=as.Date('2014-01-01')&Date<=as.Date('2019-12-31')) %>% select(Date,BVR_Vol_m3)

dvol <- vol %>% filter(Date>=as.Date('2014-01-01')&Date<=as.Date('2019-12-31')) %>% select(Date)

# Calculate dVol/dt by vol2-vol1/s
vol3 <- as.data.frame((vol2$BVR_Vol_m3 - vol1$BVR_Vol_m3)/(24*60*60))
names(vol3)[1] <- "dv_m3s"

dvol <- cbind.data.frame(dvol,vol3)

# Check change in water level
ggplot(dvol,mapping=aes(x=Date,y=dv_m3s))+
  geom_line()

# Calculate outflow as the total inflow - change in water level
outflow <- as.data.frame(inflow$FLOW-dvol$dv_m3s)
names(outflow)[1] <- "FLOW"
outflow <- cbind.data.frame(dvol,outflow)
outflow <- outflow %>% select(Date,FLOW) %>% 
  mutate_if(is.numeric,round,4) #round to 4 digits
names(outflow)[1] <- "time"

dates <- c("2019-04-29",
           "2019-05-30",
           "2019-06-27",
           "2019-07-18",
           "2019-08-22",
           "2019-09-20",
           "2019-10-04")
dates <- as.Date(dates)
rc_outflow <- outflow[as.Date(outflow$time) %in% as.Date(dates), ]

#diagnostic plot
ggplot()+
  geom_line(rc_outflow,mapping=aes(x=time,y=FLOW,color="Outflow"))+
  #geom_line(inflow,mapping=aes(x=time,y=FLOW,color="Inflow"))+
  theme_classic(base_size=15)

write.csv(rc_outflow, './Data/bvr_outflow_calcs.csv', row.names = FALSE)
