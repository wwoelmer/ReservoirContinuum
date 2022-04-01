#install.packages('rLakeAnalyzer')
library(rLakeAnalyzer)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(ggpubr)

ctd <- read.csv('./Data/raw_data/ctd_edi.csv')

ctd$Date <- as.Date(ctd$Date)
ctd$Site <- as.factor(ctd$Site)
dates <- c(as.Date('2019-04-29'), as.Date('2019-05-30'), as.Date('2019-06-27'), as.Date('2019-07-18'), 
           as.Date('2019-08-22'), as.Date('2019-09-20'), as.Date('2019-10-04'))
ctd_rc <- ctd %>% 
  filter(Reservoir=='FCR' | Reservoir=='BVR' & Site == '50') %>% 
  filter(Date %in% dates) %>% 
  filter(Depth_m > 0.5) %>% 
  select(Date, Reservoir, Site, Depth_m, Temp_C) %>% 
  filter(Date!='2019-09-20' | Reservoir!='BVR' | Depth_m < 10.16)


# YSI for BVR days without CTD
ysi <- read.csv('./Data/raw_data/spcond_edi.csv') # this include temp profiles
ysi$Date <- as.Date(ysi$Date)
ysi$Site <- as.factor(ysi$Site)

ysi_rc <- ysi %>% 
  filter(Site == '50') %>% 
  filter(Reservoir=='FCR' | Reservoir=='BVR') %>% 
  filter(Date %in% dates) %>% 
  select(Date, Reservoir, Site, Depth_m, Temp_C) 
ysi_rc <- na.omit(ysi_rc)
ysi_rc <- ysi_rc %>% 
  filter(Date < as.Date('2019-06-01')) %>% 
  distinct(Date, Reservoir, Site, Depth_m, .keep_all = TRUE)

# get rid of some weird surface measurements in BVR on 05-30
ysi_rc <- ysi_rc %>% 
  filter(Date!='2019-05-30' | Reservoir!='BVR' | Depth_m!=1.0) %>% 
  filter(Date!='2019-05-30' | Reservoir!='BVR' | Depth_m!=2.0)

# thermistor string data for FCR days without CTD
cat <- read.csv("./Data/raw_data/catwalk_edi.csv")
cat_rc <- cat %>% 
  mutate(Date = date(DateTime)) %>% 
  filter(Date==as.Date('2019-04-29') | Date==as.Date('2019-05-30')) %>% 
  select(Date, Reservoir, Site, ThermistorTemp_C_surface:ThermistorTemp_C_9) 
colnames(cat_rc) <- c("Date", "Reservoir", "Site", "0.1", "1.0", "2.0", "3.0", "4.0", "5.0", "6.0", "7.0", "8.0", "9.0")
cat_long <- cat_rc %>% 
  pivot_longer("0.1":"9.0", names_to = "Depth_m", values_to = "Temp_C")
cat_long$Depth_m <- as.numeric(cat_long$Depth_m)
cat_rc <- cat_long %>% 
  group_by(Date, Depth_m) %>% 
  mutate(Temp_C = mean(Temp_C, na.rm = TRUE)) %>% 
  distinct(Date, Reservoir, Depth_m, .keep_all = TRUE)

# put data together
temp_rc <- rbind(ctd_rc, ysi_rc)
temp_rc <- rbind(temp_rc, cat_rc)
temp_rc <- temp_rc %>% 
  distinct(Date, Reservoir, Site, Depth_m, .keep_all = TRUE)


ggplot(data = temp_rc, aes(x = Temp_C, y = Depth_m)) +
  geom_line(aes(col = Reservoir)) +
  facet_wrap(~Date) +
  scale_y_reverse()



f <- temp_rc[temp_rc$Date=='2019-09-20' & temp_rc$Reservoir=='FCR',]

metrics_rc <- temp_rc %>% 
  group_by(Date, Reservoir) %>% 
  mutate(thermo_depth = thermo.depth(Temp_C, Depth_m, seasonal = TRUE)) %>% 
  distinct(thermo_depth, Reservoir, Date, .keep_all = TRUE)

f_bathy <- load.bathy('./Data/raw_data/FCR.bth')
b_bathy <- load.bathy('./Data/raw_data/BVR.bth')

b_rc <- temp_rc[temp_rc$Reservoir=='BVR',]
ss_b <- b_rc %>% 
  group_by(Date, Reservoir) %>% 
  mutate(schmidt_stability = schmidt.stability(Temp_C, Depth_m, bthA = b_bathy$areas, bthD = b_bathy$depths)) %>% 
  distinct(schmidt_stability, Reservoir, Date, .keep_all = TRUE)

f_rc <- temp_rc[temp_rc$Reservoir=='FCR',]
ss_f <- f_rc %>% 
  group_by(Date, Reservoir) %>% 
  mutate(schmidt_stability = schmidt.stability(Temp_C, Depth_m, bthA = f_bathy$areas, bthD = f_bathy$depths)) %>% 
  distinct(schmidt_stability, Reservoir, Date, .keep_all = TRUE)

ss <- rbind(ss_b, ss_f)

metrics <- left_join(metrics_rc, ss)
metrics <- metrics %>% 
  select(Date, Reservoir, thermo_depth, schmidt_stability)
metrics <- metrics[order(metrics$Date),]

s <- ggplot(data = metrics, aes(x = Date, y =schmidt_stability, col = Reservoir)) +
  geom_line(size = 1.4) +
  ylab("Schmidt Stability (J/m^2)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text = element_text(size = 12))

t <- ggplot(data = metrics, aes(x = Date, y =thermo_depth, col = Reservoir)) +
  geom_line(size = 1.4) +
  scale_y_reverse() +
  ylab("Thermocline Depth (m)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text = element_text(size = 12))
ggarrange(t, s, common.legend = TRUE)

colnames(metrics) <- c("Date", "Reservoir", "Thermocline Depth (m)", "Schmidt Stability (J/m^2)")

write.csv(metrics, './Data/thermo_schmidt_metrics.csv', row.names = FALSE)

