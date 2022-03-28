# combine all data together into one dataset and define categorical variables
library(tidyverse)

# define specifics for RC days for subsetting
dates <- c(as.Date('2019-04-29'), as.Date('2019-05-30'), as.Date('2019-06-27'), as.Date('2019-07-18'), 
           as.Date('2019-07-22'), as.Date('2019-08-22'), as.Date('2019-09-20'), as.Date('2019-10-04'))
sites <- c('1', '20', '30', '45', '50', '100', '200', '99', '101', '102')
res <- c('FCR', 'BVR')

###################
# read in chem
chem <- read.csv('./Data/raw_data/chem_all_edi.csv')
chem$Date <- as.Date(chem$DateTime)
chem$Site <- as.factor(chem$Site)
chem <- chem %>% 
  filter(Reservoir=='FCR' | Reservoir=='BVR') %>% 
  filter(Depth_m=='0.1') %>% 
  filter(Date %in% dates) %>% 
  select(Date, Reservoir, Site, Rep, TN_ugL:DOC_mgL) 

# average reps
chem <- chem %>% 
  group_by(Date, Reservoir, Site) %>% 
  mutate(TN_ugL = ifelse((Rep==1|2), mean(TN_ugL, na.rm = TRUE), TN_ugL)) %>% 
  mutate(TP_ugL = ifelse((Rep==1|2), mean(TP_ugL, na.rm = TRUE), TP_ugL)) %>% 
  mutate(NH4_ugL = ifelse((Rep==1|2), mean(NH4_ugL, na.rm = TRUE), NH4_ugL)) %>% 
  mutate(NO3NO2_ugL = ifelse((Rep==1|2), mean(NO3NO2_ugL, na.rm = TRUE), NO3NO2_ugL)) %>% 
  mutate(SRP_ugL = ifelse((Rep==1|2), mean(SRP_ugL, na.rm = TRUE), SRP_ugL)) %>% 
  mutate(DOC_mgL = ifelse((Rep==1|2), mean(DOC_mgL, na.rm = TRUE), DOC_mgL)) 

chem <- chem %>% 
  distinct(Date, Reservoir, Site, .keep_all = TRUE) %>% 
  filter(Site!=100.1) %>% 
  select(-Rep)

# some SRP values are 0, so add very very small value to these for dividing by this value later
min_val <- min(chem[chem$SRP_ugL>0,"SRP_ugL"], na.rm = TRUE)

chem <- chem %>% 
  mutate(SRP_ugL = SRP_ugL + min_val) %>% 
  mutate(TN_TP = TN_ugL/TP_ugL) %>% 
  mutate(DP_TP = SRP_ugL/TP_ugL) %>% 
  mutate(DN_TN = (NH4_ugL + NO3NO2_ugL)/TN_ugL) %>% 
  mutate(DN_DP = (NH4_ugL + NO3NO2_ugL)/SRP_ugL)

###################
# read in chl
chl <- read.csv('./Data/raw_data/chl_all_edi.csv')
colnames(chl)[1] <- "Reservoir"
chl$Date <- as.Date(chl$DateTime)
chl$Site <- as.factor(chl$Site)
chl <- chl %>% 
  filter(Reservoir=='FCR' | Reservoir=='BVR') %>% 
  filter(Depth_m=='0.1') %>% 
  filter(Date %in% dates) %>% 
  select(Date, Reservoir, Site, Chla_ugL) 

###################
# read in inflow

inf <- read.csv('./Data/raw_data/flow_edi.csv')
inf$Date <- as.Date(inf$Date)
inf$Site <- as.factor(inf$Site)
inf <- inf %>% select(Reservoir:Flow_cms)

# reading from 05-30-2019 at F200 was done with salt slug but had very poor curve for integration, see here: https://docs.google.com/spreadsheets/d/1HB9Kq_si33m8z__UB4dcPcqlo6Zh439A/edit#gid=510435495
# which may have skewed the estimate, 
# we will use the observation from three days earlier on 05-27-2019 instead

inf$Date[inf$Reservoir=='FCR' & inf$Site=='200' & inf$Date=='2019-05-27'] <- '2019-05-30'
###################
# read in eems

eems <- read.csv('./Data/raw_data/eems_edi.csv')
eems$DateTime <- as.Date(eems$DateTime)
eems$res_site <- paste0(eems$Reservoir, '_', eems$Site)
eems <- eems[eems$Site %in% sites,]
eems <- eems[eems$Reservoir %in% res,]
eems <- eems[eems$DateTime %in% dates,]
eems <- eems %>% 
  select(Reservoir:DateTime, Rep, Dilution, A, T, HIX, BIX) %>%
  group_by(Reservoir, Site, DateTime) 
eems <- na.omit(eems)
eems <- eems %>% 
  mutate(id = paste0(Reservoir, Site, DateTime)) %>% 
  distinct(id, .keep_all = TRUE)
colnames(eems)[3] <- 'Date'
eems$Site <- as.factor(eems$Site)
eems <- eems %>% 
  select(Reservoir, Site, Date, A, T)

###################
# read in distance data from arcgis
dist <- read.csv('./Data/raw_data/rc_coordinates_100only.csv')
dist <- dist %>% select(Reservoir, Site, distance_m)
dist$Site <- as.factor(dist$Site)

###################
# read in sp conductivity data

spcond <- read.csv('./Data/raw_data/spcond_edi.csv')
spcond$Date <- as.Date(spcond$DateTime)

spcond <- spcond %>%
  filter(Reservoir=='FCR' | Reservoir=='BVR') %>% 
  filter(Depth_m=='0.1') %>% 
  filter(Date %in% dates) %>% 
  select(Date, Reservoir, Site, Sp_cond_uScm)

# there are duplicate entries with NA for some days/sites
spcond <- na.omit(spcond)
spcond$Site <- as.factor(spcond$Site)

# a couple of days we do not have sp cond data at F50, so use CTD data instead
ctd <- read.csv('./Data/raw_data/ctd_edi.csv')
ctd$Date <- as.Date(ctd$Date)
ctd$Site <- as.factor(ctd$Site)
dates_ctd <- c(as.Date('2019-06-27'), as.Date('2019-07-18'), as.Date('2019-10-04'))

ctd_sp <- ctd %>% 
  filter(Reservoir=='FCR' | Reservoir=='BVR' & Site == '50') %>%
  mutate(Depth_m = ifelse(Depth_m >= 0.1 & Depth_m <= 0.5, 0.1, Depth_m)) %>% 
  filter(Depth_m==0.1) %>% 
  filter(Date %in% dates_ctd) %>% 
  select(Date, Reservoir, Site, Spec_Cond_uScm) 

ctd_sp2 <- aggregate(Spec_Cond_uScm ~ Date + Reservoir, data = ctd_sp, mean)
ctd_sp2$Site <- '50'
colnames(ctd_sp2)[3] <- 'Sp_cond_uScm'
spcond_all <- rbind(spcond, ctd_sp2)


###################
# read in bvr outflow data
# make this FLOW for BVR 50
outflow <- read.csv('./Data/bvr_outflow_calcs.csv')
colnames(outflow) <- c('Date', 'Flow_cms')
outflow$Date <- as.Date(outflow$Date)
outflow$Reservoir <- 'BVR'
outflow$Site <- '50'
outflow$Site <- as.factor(outflow$Site)

flow <- rbind(inf, outflow)

###################
# combine datasets

all <- left_join(chem, chl)
all <- left_join(all, flow)
all <- left_join(all, eems)
all <- left_join(all, dist)
all <- left_join(all, spcond_all)


all$distance_from_stream <- 'NA'
all <- all %>% unite('res_site', Reservoir:Site, remove = FALSE)
stream_sites <- c('FCR_100', 'FCR_200', 'FCR_102', 'FCR_101', 'FCR_99', 'BVR_100', 'BVR_200', 'FCR_1')
riv_sites <- c('FCR_20', 'BVR_20', 'BVR_30') 
trans_sites <- c('BVR_1', 'BVR_45', 'FCR_30', 'FCR_45')
lac_sites <- c('FCR_50', 'BVR_50')

for (i in 1:nrow(all)) {
  if(all$res_site[i] %in% stream_sites){
    all$distance_from_stream[i] <- '0'
  }else if(all$res_site[i] %in% riv_sites){
    all$distance_from_stream[i] <- '1'
  }else if(all$res_site[i] %in% trans_sites){
    all$distance_from_stream[i] <- '2'
  }else if(all$res_site[i] %in% lac_sites){
    all$distance_from_stream[i] <- '3'
  }
}

# look for duplicates across date/reservoir/site
all <- all[!duplicated(all[c('Reservoir', 'Date', 'Site')]),]

write.csv(all, './Data/continuum_data.csv', row.names = FALSE)

