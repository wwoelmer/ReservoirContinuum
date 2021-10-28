# paper figures

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

# distance from stream for all variables figure
summ <- data %>% 
  group_by(Site) %>% 
  mutate(TN_mean = mean(TN_ugL, na.rm = TRUE),
         TP_mean = mean(TP_ugL, na.rm = TRUE),
         NH4_mean = mean(NH4_ugL, na.rm = TRUE),
         NO3_mean = mean(NO3NO2_ugL, na.rm = TRUE),
         SRP_mean = mean(SRP_ugL, na.rm = TRUE),
         DOC_mean = mean(DOC_mgL, na.rm = TRUE),
         Chl_mean = mean(Chla_ugL, na.rm = TRUE),
         A_mean = mean(A, na.rm = TRUE),
         T_mean = mean(T, na.rm = TRUE)) %>% 
  distinct(res_site, .keep_all = TRUE) %>% 
  select(Site, Reservoir, distance_from_stream, TN_mean:T_mean, distance_m) %>% 
  pivot_longer(TN_mean:T_mean, names_to = 'variable', values_to = 'value')

ggplot(data = summ, aes(x = distance_from_stream, y = value, col = as.factor(Reservoir))) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~variable, scales = "free") + 
  scale_color_manual(values = r_col) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

ggplot(data = summ, aes(x = distance_m, y = value, col = as.factor(Reservoir))) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~variable, scales = "free") + 
  scale_color_manual(values = r_col) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

long <- data %>%   
  select(Site, Reservoir, Date, distance_from_stream, TN_ugL:Chla_ugL, A:BIX, TN_TP:distance_m) %>% 
  pivot_longer(TN_ugL:DN_TN, names_to = 'variable', values_to = 'value')


levels <- c('A', 'T', 'DOC_mgL',
            'NH4_ugL', 'NO3NO2_ugL', 'SRP_ugL',
            'TN_ugL', 'TP_ugL', 'Chla_ugL',
            'HIX', 'BIX', 'TN_TP', 'DP_TP', 'DN_TN')
labels <- c('Autoch', 'Alloch', 'DOC',
            'NH4', 'NO3', 'SRP',
            'TN', 'TP', 'Chl-a',
            'HIX', 'BIX', 'TN_TP', 'DP_TP', 'DN_TN')
names(labels) <- levels
long$variable <- factor(long$variable, levels = levels)

ggplot(data = long[long$distance_from_stream > 0,], aes(x = distance_m, y = value)) +
  geom_point(aes(col = as.factor(Reservoir))) +
  geom_smooth() +
  xlab('Distance from stream') +
  ylab('Concentration') +
  facet_wrap(~variable, scales = "free", labeller = labeller(variable = labels)) + 
  scale_color_manual(values = r_col) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        #   legend.position = "none"
  )

# and with stream measurements
ggplot(data = long, aes(x = distance_m, y = value)) +
  geom_point(aes(col = as.factor(Reservoir))) +
  geom_smooth() +
  xlab('Distance from stream') +
  ylab('Concentration') +
  facet_wrap(~variable, scales = "free", labeller = labeller(variable = labels)) + 
  scale_color_manual(values = r_col) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        #   legend.position = "none"
  )

ggplot(data = long[long$distance_from_stream > 0,], aes(x = distance_from_stream, y = value)) +
  geom_point(aes(col = distance_m, shape = as.factor(Reservoir))) +
  geom_smooth() +
  xlab('Distance from stream') +
  ylab('Concentration') +
  facet_wrap(~variable, scales = "free", labeller = labeller(variable = labels)) + 
  scale_color_steps(low = 'red', high = 'blue') + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        #   legend.position = "none"
  )

ggplot(data = long[long$Reservoir=='BVR' & long$distance_from_stream > 0,], aes(x = distance_m, y = value, col = as.factor(Date))) +
  geom_smooth(aes(group = as.factor(Date))) +
  geom_point() +
  xlab('Distance from stream') +
  ylab('Concentration') +
  facet_wrap(~variable, scales = "free", labeller = labeller(variable = labels)) + 
  #scale_color_manual(values = r_col) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        #   legend.position = "none"
        )
ggplot(data = long[long$Reservoir=='FCR' & long$distance_from_stream > 0,], aes(x = distance_m, y = value, col = as.factor(Date))) +
  geom_point() +
  geom_smooth(aes(col = as.factor(Date))) +
  xlab('Distance from stream') +
  ylab('Concentration') +
  facet_wrap(~variable, scales = "free", labeller = labeller(variable = labels)) + 
  #scale_color_manual(values = r_col) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        #   legend.position = "none"
  )
no_stream <- ggplot(data = long[long$distance_from_stream > 0,], aes(x = distance_from_stream, y = value, col = as.factor(Date))) +
  geom_point(size = 6, aes(shape = as.factor(Reservoir))) +
  geom_smooth() +
  xlab('Distance from stream') +
  ylab('Concentration') +
  facet_wrap(~variable, scales = "free", labeller = labeller(variable = labels)) + 
  #scale_color_manual(values = r_col) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
     #   legend.position = "none",
        axis.text = element_text(size = 32),
        axis.title = element_text(size = 32),
        strip.text = element_text(size = 32))
no_stream

with_stream <- ggplot(data = long, aes(x = distance_from_stream, y = value, col = as.factor(Date))) +
  geom_point(size = 6, aes(shape = as.factor(Reservoir))) +
  geom_smooth(aes(col = as.factor(Date))) +
  xlab('Distance from stream') +
  ylab('Concentration') +
  #geom_violin() +
  facet_wrap(~variable, scales = "free", labeller = labeller(variable = labels)) + 
  #scale_color_manual(values = r_col) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
       # legend.position = "none",
        axis.text = element_text(size = 32),
        axis.title = element_text(size = 32),
        strip.text = element_text(size = 32))

with_stream

png("./Figures/vars_along_continuum_no_stream.png", width = 1100, height = 1100)
no_stream
dev.off()

png("./Figures/vars_along_continuum_with_stream.png", width = 1100, height = 1100)
with_stream
dev.off()


#############################################################################################################################################
# calculate heterogeneity within the reservoir (coefficient of variation) for each reservoir on each RC day

CV <- data[data$distance_from_stream>0,] %>% 
  group_by(Date, Reservoir) %>% 
  mutate(CV_chl = cv(Chla_ugL, na.rm = TRUE)) %>% 
  mutate(CV_SRP = cv(SRP_ugL, na.rm = TRUE)) %>% 
  mutate(CV_DOC = cv(DOC_mgL, na.rm = TRUE)) %>% 
  mutate(CV_NO3 = cv(NO3NO2_ugL, na.rm = TRUE)) %>% 
  mutate(CV_NH4 = cv(NH4_ugL, na.rm = TRUE)) %>% 
  mutate(CV_TP = cv(TP_ugL, na.rm = TRUE)) %>% 
  mutate(CV_TN = cv(TN_ugL, na.rm = TRUE)) %>% 
  mutate(CV_TNTP = cv(TN_TP, na.rm = TRUE)) %>% 
  mutate(CV_A = cv(A, na.rm = TRUE)) %>% 
  mutate(CV_T = cv(T, na.rm = TRUE)) %>% 
  select(Date, Reservoir, CV_chl:CV_T) %>% 
  distinct(Date, Reservoir, .keep_all = TRUE)
CV$Date <- as.Date(CV$Date)

# what is a meaningful difference in CV?
# t-test btw each time series for each variable?
bvr <- CV[CV$Reservoir=='BVR',]
fcr <- CV[CV$Reservoir=='FCR',]
t.test(fcr$CV_chl, bvr$CV_chl) # significant
wilcox.test(fcr$CV_chl, bvr$CV_chl) # significant

t.test(fcr$CV_SRP, bvr$CV_SRP) # NS
wilcox.test(fcr$CV_SRP, bvr$CV_SRP) # NS

t.test(fcr$CV_NO3, bvr$CV_NO3) # marginally significant
wilcox.test(fcr$CV_NO3, bvr$CV_NO3) # marginally significant

t.test(fcr$CV_NH4, bvr$CV_NH4) # significant
wilcox.test(fcr$CV_NH4, bvr$CV_NH4) # significant

t.test(fcr$CV_TN, bvr$CV_TN) #NS
wilcox.test(fcr$CV_TN, bvr$CV_TN) #NS

t.test(fcr$CV_TP, bvr$CV_TP) #NS
wilcox.test(fcr$CV_TP, bvr$CV_TP) #NS

t.test(fcr$CV_TNTP, bvr$CV_TNTP) # NS
wilcox.test(fcr$CV_TNTP, bvr$CV_TNTP) # NS

t.test(fcr$CV_T, bvr$CV_T) # NS
wilcox.test(fcr$CV_T, bvr$CV_T) # NS

t.test(fcr$CV_A, bvr$CV_A) #NS
wilcox.test(fcr$CV_A, bvr$CV_A) #NS

t.test(fcr$CV_DOC, bvr$CV_DOC) #NS
wilcox.test(fcr$CV_DOC, bvr$CV_DOC) #NS

# after checking hist(CV$CV_XX) no variables are normally distributed so will use wilcox test

chl <- ggplot(data = CV, aes(x = Date, y = CV_chl, col = Reservoir)) + geom_line(size = 1.2) +
  theme(legend.position = 'none',
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        strip.text = element_text(size = 18)) + 
  annotate('text', x = as.Date('2019-08-20'), y = 0.3, label = '**', size = 10) +  
  scale_color_manual(values = r_col) 

srp <- ggplot(data = CV, aes(x = Date, y = CV_SRP, col = Reservoir)) + geom_line(size = 1.2) +
  scale_color_manual(values = r_col) + 
  theme(legend.position = 'none',
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        strip.text = element_text(size = 18))   
  
no3 <- ggplot(data = CV, aes(x = Date, y = CV_NO3, col = Reservoir)) + geom_line(size = 1.2) +
  scale_color_manual(values = r_col) + 
  theme(legend.position = 'none',
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        strip.text = element_text(size = 18)) +
  annotate('text', x = as.Date('2019-08-20'), y = 1.75, label = '**', size = 10)   
  

nh4 <- ggplot(data = CV, aes(x = Date, y = CV_NH4, col = Reservoir)) + 
  geom_line(size = 1.2) +
  scale_color_manual(values = r_col) + 
  theme(legend.position = 'none',
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        strip.text = element_text(size = 18)) +
  annotate('text', x = as.Date('2019-08-20'), y = 0.875, label = '**', size = 10) 
  

tp <- ggplot(data = CV, aes(x = Date, y = CV_TP, col = Reservoir)) + geom_line(size = 1.2) +
  scale_color_manual(values = r_col) + 
  theme(legend.position = 'none',
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        strip.text = element_text(size = 18))
tn <- ggplot(data = CV, aes(x = Date, y = CV_TN, col = Reservoir)) + geom_line(size = 1.2) +
  scale_color_manual(values = r_col) + 
  theme(legend.position = 'none',
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        strip.text = element_text(size = 18))
tntp <- ggplot(data = CV, aes(x = Date, y = CV_TNTP, col = Reservoir)) + geom_line(size = 1.5) +
  scale_color_manual(values = r_col) + 
  theme(legend.position = 'none',
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        strip.text = element_text(size = 18))
a_peak <- ggplot(data = CV, aes(x = Date, y = CV_A, col = Reservoir)) + geom_line(size = 1.2) +
  scale_color_manual(values = r_col) + 
  theme(legend.position = 'none',
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        strip.text = element_text(size = 18))

t_peak <- ggplot(data = CV, aes(x = Date, y = CV_T, col = Reservoir)) + geom_line(size = 1.2) +
  scale_color_manual(values = r_col) + 
  theme(legend.position = 'none',
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        strip.text = element_text(size = 18))

doc <- ggplot(data = CV, aes(x = Date, y = CV_DOC, col = Reservoir)) + geom_line(size = 1.2) +
  scale_color_manual(values = r_col) + 
  theme(legend.position = 'none',
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        strip.text = element_text(size = 18))

png("./Figures/CV_vs_time.png", width = 1100, height = 1100)
(a_peak + chl + doc)/(nh4 + no3 + srp)/(t_peak + tn + tp) 
dev.off()


# bring in water retention time as driver of CV
rt <- read.csv('./Data/unscripted_files/20201110_WRT.csv')
rt$Reservoir <- 'NA'
for (i in 1:nrow(rt)) {
  if(rt$Site[i]=='299'){
    rt$Reservoir[i] <- 'FCR'
  }else if(rt$Site[i]=='300'){
    rt$Reservoir[i] <- 'BVR'
  }
}
rt <- rt %>% select(Date, Reservoir, wrt_d)
rt$Date <- as.Date(rt$Date)
CV <- left_join(CV, rt)

# hard code in the Jun 20 WRT for BVR as the Jun 27 WRT (since this is the closest obs)
CV[5,13] <- 861.73327 #value taken from rt dataset for 20 Jun 19 at BVR
CV$Notes <- 'NA'
CV[5,14] <- 'WRT from 20 Jun 19'

cv_long <- CV %>%   
  select(Date, Reservoir, wrt_d, Notes, CV_chl:CV_T, -CV_TNTP) %>% 
  pivot_longer(CV_chl:CV_T, names_to = 'variable', values_to = 'value')

levels <- c('CV_A', 'CV_T', 'CV_DOC',
            'CV_NH4', 'CV_NO3', 'CV_SRP',
            'CV_TN', 'CV_TP', 'CV_chl')
labels <- c('Autoch', 'Alloch', 'DOC',
            'NH4', 'NO3', 'SRP',
            'TN', 'TP', 'Chl-a')
names(labels) <- levels
cv_long$variable <- factor(cv_long$variable, levels = levels)

p <- ggplot(data = cv_long, aes(x = wrt_d, y = value)) +
  facet_wrap(~variable, scales = "free", labeller = labeller(variable = labels)) + 
  geom_point(aes(col = Reservoir), size = 10) +
  scale_color_manual(values = r_col) + 
  geom_smooth(method = 'lm', se = FALSE, col = 'black', size = 2) +
  theme_bw() +
  xlab('Retention Time (days)') +
  ylab('Coefficient of variation') +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        axis.text = element_text(size = 32),
        axis.title = element_text(size = 32),
        strip.text = element_text(size = 32)) +
  stat_poly_eq(
               aes(label = ..rr.label..), 
               size = 15, 
               parse = TRUE,
               label.x.npc = "right",
               vstep = 0.05) # sets vertical spacing

  #stat_regline_equation(aes(label = ..eq.label..))# +
  #stat_regline_equation(aes(label = ..rr.label..), size = 7)
p

png("./Figures/WRT_vs_CV.png", width = 1100, height = 1100)
p
dev.off()


