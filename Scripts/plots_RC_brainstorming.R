library(tidyverse)
library(patchwork)
library(lubridate)
library(EnvStats)

#data <- read.csv('./Data/continuum_pf.csv')
data <- read.csv('./Data/continuum_ww.csv')
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

ggplot(data = data[data$Site %in% in_reservoir,], aes(x = Date, y = Chla_ugL, col = as.factor(Reservoir))) + facet_wrap(~Site) + geom_line()
ggplot(data = data[data$Site %in% in_stream,], aes(x = Date, y = Chla_ugL, col = as.factor(Reservoir))) + facet_wrap(~Site) + geom_line()


plot(data$DN_TN, data$DP_TP)
ggplot(data = data, aes(x = Date, y = TN_TP, col = Reservoir)) + facet_wrap(~Site) + geom_line() 
ggplot(data = data, aes(x = Date, y = DN_TN, col = Reservoir)) + facet_wrap(~Site) + geom_line() 
ggplot(data = data, aes(x = Date, y = DP_TP, col = Reservoir)) + facet_wrap(~Site) + geom_line() 


ggplot(data = data, aes(x = Date, y = TN_ugL, col = as.factor(Site))) + facet_wrap(~Reservoir) + geom_line()
ggplot(data = data, aes(x = Date, y = TP_ugL, col = as.factor(Site))) + facet_wrap(~Reservoir) + geom_line()
ggplot(data = data, aes(x = Date, y = NH4_ugL, col = as.factor(Site))) + facet_wrap(~Reservoir) + geom_line()
ggplot(data = data, aes(x = Date, y = SRP_ugL, col = as.factor(Site))) + facet_wrap(~Reservoir) + geom_line()
ggplot(data = data, aes(x = Date, y = NO3NO2_ugL, col = as.factor(Site))) + facet_wrap(~Reservoir) + geom_line()

ggplot(data[data$Site=='102',], aes(x = Date, y = connectivity)) + geom_point(size = 3) + theme(axis.text = element_text(size = 14))

ggplot(data = data[data$Site %in% in_reservoir,], aes(x = connectivity, y = Chla_ugL, col = as.factor(Site))) + facet_wrap(~Reservoir) + geom_point()
ggplot(data = data[data$Site %in% in_reservoir,], aes(x = connectivity, y = TN_ugL, col = as.factor(Site))) + facet_wrap(~Reservoir) + geom_point()
ggplot(data = data[data$Site %in% in_reservoir,], aes(x = connectivity, y = TP_ugL, col = as.factor(Site))) + facet_wrap(~Reservoir) + geom_point()
ggplot(data = data[data$Site %in% in_reservoir,], aes(x = connectivity, y = TN_TP, col = as.factor(Site))) + facet_wrap(~Reservoir) + geom_point()
ggplot(data = data[data$Site %in% in_reservoir,], aes(x = connectivity, y = SRP_ugL, col = as.factor(Site))) + facet_wrap(~Reservoir) + geom_point()
ggplot(data = data[data$Site %in% in_reservoir,], aes(x = connectivity, y = NH4_ugL, col = as.factor(Site))) + facet_wrap(~Reservoir) + geom_point()
ggplot(data = data[data$Site %in% in_reservoir,], aes(x = connectivity, y = NO3NO2_ugL, col = as.factor(Site))) + facet_wrap(~Reservoir) + geom_point()

ggplot(data = data[data$Site %in% in_stream,], aes(x = connectivity, y = Chla_ugL, col = as.factor(Site))) + facet_wrap(~Reservoir) + geom_point()
ggplot(data = data[data$Site %in% in_stream,], aes(x = connectivity, y = TN_ugL, col = as.factor(Site))) + facet_wrap(~Reservoir) + geom_point()
ggplot(data = data[data$Site %in% in_stream,], aes(x = connectivity, y = TP_ugL, col = as.factor(Site))) + facet_wrap(~Reservoir) + geom_point()
ggplot(data = data[data$Site %in% in_stream,], aes(x = connectivity, y = TN_TP, col = as.factor(Site))) + facet_wrap(~Reservoir) + geom_point()
ggplot(data = data[data$Site %in% in_stream,], aes(x = connectivity, y = SRP_ugL, col = as.factor(Site))) + facet_wrap(~Reservoir) + geom_point()
ggplot(data = data[data$Site %in% in_stream,], aes(x = connectivity, y = NH4_ugL, col = as.factor(Site))) + facet_wrap(~Reservoir) + geom_point()
ggplot(data = data[data$Site %in% in_stream,], aes(x = connectivity, y = NO3NO2_ugL, col = as.factor(Site))) + facet_wrap(~Reservoir) + geom_point()

a <- ggplot(data = data[data$Site %in% in_reservoir,], aes(x = Date, y = TN_ugL, col = as.factor(Site))) + 
  facet_wrap(~Reservoir) + 
  geom_line() + 
  theme(legend.position = 'none',
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 14))
b <- ggplot(data = data[data$Site %in% in_reservoir,], aes(x = Date, y = TP_ugL, col = as.factor(Site))) + 
  facet_wrap(~Reservoir) + 
  geom_line() +
  theme(legend.position = 'none',
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 14))
c <- ggplot(data = data[data$Site %in% in_reservoir,], aes(x = Date, y = NH4_ugL, col = as.factor(Site))) + 
  facet_wrap(~Reservoir) + 
  geom_line() +
  theme(legend.position = 'none',
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 14))
d <- ggplot(data = data[data$Site %in% in_reservoir,], aes(x = Date, y = SRP_ugL, col = as.factor(Site))) + 
  facet_wrap(~Reservoir) + 
  geom_line() +
  theme(legend.position = 'none',
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 14))
e <- ggplot(data = data[data$Site %in% in_reservoir,], aes(x = Date, y = NO3NO2_ugL, col = as.factor(Site))) + 
  facet_wrap(~Reservoir) + 
  geom_line() +
  theme(legend.position = 'none',
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 14))
f <- ggplot(data = data[data$Site %in% in_reservoir,], aes(x = Date, y = TN_TP, col = as.factor(Site))) + 
  facet_wrap(~Reservoir) + 
  geom_line() + 
  theme(legend.position = 'none',
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 14))
g <- ggplot(data = data[data$Site %in% in_reservoir,], aes(x = Date, y = Chla_ugL, col = as.factor(Site))) + 
  facet_wrap(~Reservoir) + 
  geom_line() +
  theme(legend.position = 'none',
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        strip.text = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20))

png("./Figures/reservoir continuum/Nutrients_BVR_FCR_RC_Days.png", width = 1100, height = 800)
(a + e + c)/(b + d + f )
dev.off()

png("./Figures/reservoir continuum/Chla_BVR_FCR_RC_Days.png", width = 1100, height = 800)
g
dev.off()


# plots along the continuum
hf <- ggplot(data = data[data$Reservoir=='FCR',], aes(x = distance_from_stream, y = Chla_ugL))  +
  geom_point(col = 'blue' ) +
  facet_wrap(~month(Date)) + 
  geom_smooth(method = 'lm') +
  theme(axis.text = element_text(size = 14),
                                    axis.title = element_text(size = 14),
                                    strip.text = element_text(size = 14))
hb <- ggplot(data = data[data$Reservoir=='BVR',], aes(x = distance_from_stream, y = Chla_ugL))  +
  geom_point(col = 'red' ) +
  facet_wrap(~month(Date)) + 
  geom_smooth(method = 'lm') +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 14))

hb
hf

i <- ggplot(data = data, aes(x = distance_from_stream, y = TN_TP, col = Reservoir ))  +
  geom_point() +
  facet_wrap(~month(Date)) + 
  geom_smooth(method = 'lm')+
  theme(#legend.position = 'none',
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 14))
j <- ggplot(data = data, aes(x = distance_from_stream, y = TN_ugL, col = Reservoir ))  +
  geom_point() +
  facet_wrap(~month(Date)) + 
  geom_smooth(method = 'lm') +
  theme(legend.position = 'none',
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 14))
k <- ggplot(data = data, aes(x = distance_from_stream, y = TP_ugL, col = Reservoir ))  +
  geom_point() +
  facet_wrap(~month(Date)) + 
  geom_smooth(method = 'lm') +
  theme(legend.position = 'none',
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 14))
l <- ggplot(data = data, aes(x = distance_from_stream, y = SRP_ugL, col = Reservoir ))  +
  geom_point() +
  facet_wrap(~month(Date)) + 
  geom_smooth(method = 'lm') +
  theme(legend.position = 'none',
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 14))
m <- ggplot(data = data, aes(x = distance_from_stream, y = NH4_ugL, col = Reservoir ))  +
  geom_point() +
  facet_wrap(~month(Date)) + 
  geom_smooth(method = 'lm') +theme(legend.position = 'none',
                                    axis.text = element_text(size = 14),
                                    axis.title = element_text(size = 14),
                                    strip.text = element_text(size = 14))
n <- ggplot(data = data, aes(x = distance_from_stream, y = NO3NO2_ugL, col = Reservoir ))  +
  geom_point() +
  facet_wrap(~month(Date)) + 
  geom_smooth(method = 'lm') +
  theme(legend.position = 'none',
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 14))

png("./Figures/reservoir continuum/Continuum_longitudinal_nutrients.png", width = 1100, height = 800)
(j + n + m)/(k + l + h )
dev.off()

######################################################################################################################
# calculate heterogeneity within the reservoir (coefficient of variation) for each reservoir on each RC day

CV <- data[data$distance_from_stream>0 & data$Site>1,] %>% group_by(Date, Reservoir) %>% 
  mutate(CV_chl = cv(Chla_ugL, na.rm = TRUE)) %>% 
  mutate(CV_SRP = cv(SRP_ugL, na.rm = TRUE)) %>% 
  mutate(CV_NO3 = cv(NO3NO2_ugL, na.rm = TRUE)) %>% 
  mutate(CV_NH4 = cv(NH4_ugL, na.rm = TRUE)) %>% 
  mutate(CV_TP = cv(TP_ugL, na.rm = TRUE)) %>% 
  mutate(CV_TN = cv(TN_ugL, na.rm = TRUE)) %>% 
  mutate(CV_TNTP = cv(TN_TP, na.rm = TRUE)) %>% 
  select(Date, Reservoir, CV_chl:CV_TNTP) %>% 
  distinct(Date, Reservoir, .keep_all = TRUE)
CV$Date <- as.Date(CV$Date)

# what is a meaningful difference in CV?
# t-test btw each time series for each variable?
bvr <- CV[CV$Reservoir=='BVR',]
fcr <- CV[CV$Reservoir=='FCR',]
t.test(fcr$CV_chl, bvr$CV_chl)
t.test(fcr$CV_SRP, bvr$CV_SRP)
t.test(fcr$CV_NO3, bvr$CV_NO3)
t.test(fcr$CV_NH4, bvr$CV_NH4)
t.test(fcr$CV_TN, bvr$CV_TN)
t.test(fcr$CV_TP, bvr$CV_TP)
t.test(fcr$CV_TNTP, bvr$CV_TNTP)


chl <- ggplot(data = CV, aes(x = Date, y = CV_chl, col = Reservoir)) + geom_line() +
  theme(legend.position = 'none',
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        strip.text = element_text(size = 18)) + 
  annotate('text', x = as.Date('2019-08-20'), y = 0.33, label = '**', size = 10)
srp <- ggplot(data = CV, aes(x = Date, y = CV_SRP, col = Reservoir)) + geom_line() +
  theme(legend.position = 'none',
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        strip.text = element_text(size = 18))
no3 <- ggplot(data = CV, aes(x = Date, y = CV_NO3, col = Reservoir)) + geom_line() +
  theme(legend.position = 'none',
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        strip.text = element_text(size = 18)) +
  annotate('text', x = as.Date('2019-08-20'), y = 2.0, label = '**', size = 10)

nh4 <- ggplot(data = CV, aes(x = Date, y = CV_NH4, col = Reservoir)) + geom_line() +
  theme(legend.position = 'none',
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        strip.text = element_text(size = 18)) +
  annotate('text', x = as.Date('2019-08-20'), y = 0.97, label = '**', size = 10)

tp <- ggplot(data = CV, aes(x = Date, y = CV_TP, col = Reservoir)) + geom_line() +
  theme(legend.position = 'none',
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        strip.text = element_text(size = 18))
tn <- ggplot(data = CV, aes(x = Date, y = CV_TN, col = Reservoir)) + geom_line() +
  theme(legend.position = 'none',
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        strip.text = element_text(size = 18))
tntp <- ggplot(data = CV, aes(x = Date, y = CV_TNTP, col = Reservoir)) + geom_line() +
  theme(legend.position = 'none',
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        strip.text = element_text(size = 18))
png("./Figures/reservoir continuum/CV_nuts_chl.png", width = 1100, height = 800)
(tn + no3 + nh4)/(tp + srp + chl) 
dev.off()

# build regression model btw CV and month?
# build regression model btw RT and month?
rt <- read.csv('./Data/continuum_discharge/20201110_WRT.csv')
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
CV[5,10] <- 861.73327 #value taken from rt dataset for 20 Jun 19 at BVR
CV$Notes <- 'NA'
CV[5,11] <- 'WRT from 20 Jun 19'

chl_rt <- ggplot(data = CV, aes(x = wrt_d, y = CV_chl, col = Reservoir)) + geom_line() +
  theme(legend.position = 'none',
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        strip.text = element_text(size = 18))
srp_rt <- ggplot(data = CV, aes(x = wrt_d, y = CV_SRP, col = Reservoir)) + geom_line() +
  theme(legend.position = 'none',
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        strip.text = element_text(size = 18))
no3_rt <- ggplot(data = CV, aes(x = wrt_d, y = CV_NO3, col = Reservoir)) + geom_line() +
  theme(legend.position = 'none',
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        strip.text = element_text(size = 18))
nh4_rt <- ggplot(data = CV, aes(x = wrt_d, y = CV_NH4, col = Reservoir)) + geom_line() +
  theme(legend.position = 'none',
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        strip.text = element_text(size = 18)) 
tp_rt <- ggplot(data = CV, aes(x = wrt_d, y = CV_TP, col = Reservoir)) + geom_line() +
  theme(legend.position = 'none',
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        strip.text = element_text(size = 18))
tn_rt <- ggplot(data = CV, aes(x = wrt_d, y = CV_TN, col = Reservoir)) + geom_line() +
  theme(legend.position = 'none',
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        strip.text = element_text(size = 18))
tntp_rt <- ggplot(data = CV, aes(x = wrt_d, y = CV_TNTP, col = Reservoir)) + geom_line() +
  theme(legend.position = 'none',
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        strip.text = element_text(size = 18))
png("./Figures/reservoir continuum/WRT_vs_CV.png", width = 1100, height = 800)
(tn_rt + no3_rt + nh4_rt)/(tp_rt + srp_rt + chl_rt) 
dev.off()

#####################################################################################
# calculate synchrony between FCR and BVR (pearson correlation coefficient)
data_bvr <- data[data$Reservoir=='BVR' & data$Site %in% in_reservoir,]
data_fcr <- data[data$Reservoir=='FCR' & data$Site %in% in_reservoir,]
cor_pearson <- data.frame('variable' = c('TN', 'NO3', 'NH4', 'TP', 'SRP', 'Chl-a', 'TNTP', 'DNTN', 'DPTP'),
                          'pearson_r' = rep(NA))

# correlations between all sites at BVR vs FCR
cor_pearson[6, 2] <- cor(data_fcr$Chla_ugL, data_bvr$Chla_ugL)
cor_pearson[5, 2] <- cor(data_fcr$SRP_ugL, data_bvr$SRP_ugL)
cor_pearson[2, 2] <- cor(data_fcr$NO3NO2_ugL, data_bvr$NO3NO2_ugL)
cor_pearson[3, 2] <- cor(data_fcr$NH4_ugL, data_bvr$NH4_ugL, use = 'complete.obs')
cor_pearson[1, 2] <- cor(data_fcr$TN_ugL, data_bvr$TN_ugL, use = 'complete.obs')
cor_pearson[4, 2] <- cor(data_fcr$TP_ugL, data_bvr$TP_ugL, use = 'complete.obs')
cor_pearson[7, 2] <- cor(data_fcr$TN_TP, data_bvr$TN_TP, use = 'complete.obs')
cor_pearson[8, 2] <- cor(data_fcr$DN_TN, data_bvr$DN_TN, use = 'complete.obs')
cor_pearson[9, 2] <- cor(data_fcr$DP_TP, data_bvr$DP_TP, use = 'complete.obs')

png("./Figures/reservoir continuum/Pearsons_r.png", width = 1200, height = 1100)
ggplot(data = cor_pearson, aes(x = variable, y = pearson_r, fill = pearson_r > 0.5)) + geom_bar(stat = 'identity') +
  theme(axis.text = element_text(size = 35),
        axis.title = element_text(size = 35),
        strip.text = element_text(size = 35)) +
  scale_fill_manual('Above 0.5', values = c('gray45', 'coral')) 
dev.off()

# now do this individually for each site pairing
#########################
# f20 and b20
cor_pearson_f20_b20 <- data.frame('variable' = c('TN', 'NO3', 'NH4', 'TP', 'SRP', 'Chl-a', 'TNTP', 'DNTN', 'DPTP'),
                          'pearson_r' = rep(NA))


bvr_20 <- data_bvr[data_bvr$Site=='20',]
fcr_20 <- data_fcr[data_fcr$Site=='20',]
cor_pearson_f20_b20[6, 2] <- cor(fcr_20$Chla_ugL,   bvr_20$Chla_ugL)
cor_pearson_f20_b20[5, 2] <- cor(fcr_20$SRP_ugL,    bvr_20$SRP_ugL)
cor_pearson_f20_b20[2, 2] <- cor(fcr_20$NO3NO2_ugL, bvr_20$NO3NO2_ugL)
cor_pearson_f20_b20[3, 2] <- cor(fcr_20$NH4_ugL,    bvr_20$NH4_ugL, use = 'complete.obs')
cor_pearson_f20_b20[1, 2] <- cor(fcr_20$TN_ugL,     bvr_20$TN_ugL, use = 'complete.obs')
cor_pearson_f20_b20[4, 2] <- cor(fcr_20$TP_ugL,     bvr_20$TP_ugL, use = 'complete.obs')
cor_pearson_f20_b20[7, 2] <- cor(fcr_20$TN_TP,      bvr_20$TN_TP, use = 'complete.obs')
cor_pearson_f20_b20[8, 2] <- cor(fcr_20$DN_TN,      bvr_20$DN_TN, use = 'complete.obs')
cor_pearson_f20_b20[9, 2] <- cor(fcr_20$DP_TP,      bvr_20$DP_TP, use = 'complete.obs')
f20b20 <- ggplot(data = cor_pearson_f20_b20, aes(x = variable, y = pearson_r, fill = pearson_r > 0.5)) + geom_bar(stat = 'identity') +
  theme(legend.position = 'none',
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 35),
        strip.text = element_text(size = 35),
        plot.title = element_text(size = 40)) +
  ggtitle('F20 and B20')+
  scale_fill_manual('Above 0.5', values = c('gray45', 'coral')) 

#################
# f20 and b30
cor_pearson_f20_b30 <- data.frame('variable' = c('TN', 'NO3', 'NH4', 'TP', 'SRP', 'Chl-a', 'TNTP', 'DNTN', 'DPTP'),
                                  'pearson_r' = rep(NA))


bvr_30 <- data_bvr[data_bvr$Site=='30',]
fcr_20 <- data_fcr[data_fcr$Site=='20',]
cor_pearson_f20_b30[6, 2] <- cor(fcr_20$Chla_ugL,   bvr_30$Chla_ugL)
cor_pearson_f20_b30[5, 2] <- cor(fcr_20$SRP_ugL,    bvr_30$SRP_ugL)
cor_pearson_f20_b30[2, 2] <- cor(fcr_20$NO3NO2_ugL, bvr_30$NO3NO2_ugL)
cor_pearson_f20_b30[3, 2] <- cor(fcr_20$NH4_ugL,    bvr_30$NH4_ugL, use = 'complete.obs')
cor_pearson_f20_b30[1, 2] <- cor(fcr_20$TN_ugL,     bvr_30$TN_ugL, use = 'complete.obs')
cor_pearson_f20_b30[4, 2] <- cor(fcr_20$TP_ugL,     bvr_30$TP_ugL, use = 'complete.obs')
cor_pearson_f20_b30[7, 2] <- cor(fcr_20$TN_TP,      bvr_30$TN_TP, use = 'complete.obs')
cor_pearson_f20_b30[8, 2] <- cor(fcr_20$DN_TN,      bvr_30$DN_TN, use = 'complete.obs')
cor_pearson_f20_b30[9, 2] <- cor(fcr_20$DP_TP,      bvr_30$DP_TP, use = 'complete.obs')
f20b30 <- ggplot(data = cor_pearson_f20_b30, aes(x = variable, y = pearson_r, fill = pearson_r > 0.5)) + geom_bar(stat = 'identity') +
  theme(legend.position = 'none', 
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 35),
        strip.text = element_text(size = 35),
        plot.title = element_text(size = 40)) +
  ggtitle('F20 and B30')+
  scale_fill_manual('Above 0.5', values = c('gray45', 'coral')) 

#################
# f20 and b30
cor_pearson_f20_b30 <- data.frame('variable' = c('TN', 'NO3', 'NH4', 'TP', 'SRP', 'Chl-a', 'TNTP', 'DNTN', 'DPTP'),
                                  'pearson_r' = rep(NA))


bvr_30 <- data_bvr[data_bvr$Site=='30',]
fcr_20 <- data_fcr[data_fcr$Site=='20',]
cor_pearson_f20_b30[6, 2] <- cor(fcr_20$Chla_ugL,   bvr_30$Chla_ugL)
cor_pearson_f20_b30[5, 2] <- cor(fcr_20$SRP_ugL,    bvr_30$SRP_ugL)
cor_pearson_f20_b30[2, 2] <- cor(fcr_20$NO3NO2_ugL, bvr_30$NO3NO2_ugL)
cor_pearson_f20_b30[3, 2] <- cor(fcr_20$NH4_ugL,    bvr_30$NH4_ugL, use = 'complete.obs')
cor_pearson_f20_b30[1, 2] <- cor(fcr_20$TN_ugL,     bvr_30$TN_ugL, use = 'complete.obs')
cor_pearson_f20_b30[4, 2] <- cor(fcr_20$TP_ugL,     bvr_30$TP_ugL, use = 'complete.obs')
cor_pearson_f20_b30[7, 2] <- cor(fcr_20$TN_TP,      bvr_30$TN_TP, use = 'complete.obs')
cor_pearson_f20_b30[8, 2] <- cor(fcr_20$DN_TN,      bvr_30$DN_TN, use = 'complete.obs')
cor_pearson_f20_b30[9, 2] <- cor(fcr_20$DP_TP,      bvr_30$DP_TP, use = 'complete.obs')
f20b30 <- ggplot(data = cor_pearson_f20_b30, aes(x = variable, y = pearson_r, fill = pearson_r > 0.5)) + geom_bar(stat = 'identity') +
  theme(legend.position = 'none',
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 35),
        strip.text = element_text(size = 35),
        plot.title = element_text(size = 40)) +
  ggtitle('F20 and B30')+
  scale_fill_manual('Above 0.5', values = c('gray45', 'coral')) 

#################
# f30 and b20
cor_pearson_f30_b20 <- data.frame('variable' = c('TN', 'NO3', 'NH4', 'TP', 'SRP', 'Chl-a', 'TNTP', 'DNTN', 'DPTP'),
                                  'pearson_r' = rep(NA))


bvr_20 <- data_bvr[data_bvr$Site=='20',]
fcr_30 <- data_fcr[data_fcr$Site=='30',]
cor_pearson_f30_b20[6, 2] <- cor(fcr_30$Chla_ugL,   bvr_20$Chla_ugL)
cor_pearson_f30_b20[5, 2] <- cor(fcr_30$SRP_ugL,    bvr_20$SRP_ugL)
cor_pearson_f30_b20[2, 2] <- cor(fcr_30$NO3NO2_ugL, bvr_20$NO3NO2_ugL)
cor_pearson_f30_b20[3, 2] <- cor(fcr_30$NH4_ugL,    bvr_20$NH4_ugL, use = 'complete.obs')
cor_pearson_f30_b20[1, 2] <- cor(fcr_30$TN_ugL,     bvr_20$TN_ugL, use = 'complete.obs')
cor_pearson_f30_b20[4, 2] <- cor(fcr_30$TP_ugL,     bvr_20$TP_ugL, use = 'complete.obs')
cor_pearson_f30_b20[7, 2] <- cor(fcr_30$TN_TP,      bvr_20$TN_TP, use = 'complete.obs')
cor_pearson_f30_b20[8, 2] <- cor(fcr_30$DN_TN,      bvr_20$DN_TN, use = 'complete.obs')
cor_pearson_f30_b20[9, 2] <- cor(fcr_30$DP_TP,      bvr_20$DP_TP, use = 'complete.obs')
f30b20 <- ggplot(data = cor_pearson_f30_b20, aes(x = variable, y = pearson_r, fill = pearson_r > 0.5)) + geom_bar(stat = 'identity') +
  theme(legend.position = 'none',
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 35),
        strip.text = element_text(size = 35),
        plot.title = element_text(size = 40)) +
  ggtitle('F30 and B20')+
  scale_fill_manual('Above 0.5', values = c('gray45', 'coral')) 

#################
# f30 and b30
cor_pearson_f30_b30 <- data.frame('variable' = c('TN', 'NO3', 'NH4', 'TP', 'SRP', 'Chl-a', 'TNTP', 'DNTN', 'DPTP'),
                                  'pearson_r' = rep(NA))


bvr_30 <- data_bvr[data_bvr$Site=='30',]
fcr_30 <- data_fcr[data_fcr$Site=='30',]
cor_pearson_f30_b30[6, 2] <- cor(fcr_30$Chla_ugL,   bvr_30$Chla_ugL)
cor_pearson_f30_b30[5, 2] <- cor(fcr_30$SRP_ugL,    bvr_30$SRP_ugL)
cor_pearson_f30_b30[2, 2] <- cor(fcr_30$NO3NO2_ugL, bvr_30$NO3NO2_ugL)
cor_pearson_f30_b30[3, 2] <- cor(fcr_30$NH4_ugL,    bvr_30$NH4_ugL, use = 'complete.obs')
cor_pearson_f30_b30[1, 2] <- cor(fcr_30$TN_ugL,     bvr_30$TN_ugL, use = 'complete.obs')
cor_pearson_f30_b30[4, 2] <- cor(fcr_30$TP_ugL,     bvr_30$TP_ugL, use = 'complete.obs')
cor_pearson_f30_b30[7, 2] <- cor(fcr_30$TN_TP,      bvr_30$TN_TP, use = 'complete.obs')
cor_pearson_f30_b30[8, 2] <- cor(fcr_30$DN_TN,      bvr_30$DN_TN, use = 'complete.obs')
cor_pearson_f30_b30[9, 2] <- cor(fcr_30$DP_TP,      bvr_30$DP_TP, use = 'complete.obs')
f30b30 <- ggplot(data = cor_pearson_f30_b30, aes(x = variable, y = pearson_r, fill = pearson_r > 0.5)) + geom_bar(stat = 'identity') +
  theme(legend.position = 'none',
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 35),
        strip.text = element_text(size = 35),
        plot.title = element_text(size = 40)) +
  ggtitle('F30 and B30')+
  scale_fill_manual('Above 0.5', values = c('gray45', 'coral')) 

#################
# f45 and b45
cor_pearson_f45_b45 <- data.frame('variable' = c('TN', 'NO3', 'NH4', 'TP', 'SRP', 'Chl-a', 'TNTP', 'DNTN', 'DPTP'),
                                  'pearson_r' = rep(NA))


bvr_45 <- data_bvr[data_bvr$Site=='45',]
fcr_45 <- data_fcr[data_fcr$Site=='45',]
cor_pearson_f45_b45[6, 2] <- cor(fcr_45$Chla_ugL,   bvr_45$Chla_ugL)
cor_pearson_f45_b45[5, 2] <- cor(fcr_45$SRP_ugL,    bvr_45$SRP_ugL)
cor_pearson_f45_b45[2, 2] <- cor(fcr_45$NO3NO2_ugL, bvr_45$NO3NO2_ugL) # sd of BVR NO3 is zero, all values are 3 ug/L
cor_pearson_f45_b45[3, 2] <- cor(fcr_45$NH4_ugL,    bvr_45$NH4_ugL, use = 'complete.obs')
cor_pearson_f45_b45[1, 2] <- cor(fcr_45$TN_ugL,     bvr_45$TN_ugL, use = 'complete.obs')
cor_pearson_f45_b45[4, 2] <- cor(fcr_45$TP_ugL,     bvr_45$TP_ugL, use = 'complete.obs')
cor_pearson_f45_b45[7, 2] <- cor(fcr_45$TN_TP,      bvr_45$TN_TP, use = 'complete.obs')
cor_pearson_f45_b45[8, 2] <- cor(fcr_45$DN_TN,      bvr_45$DN_TN, use = 'complete.obs')
cor_pearson_f45_b45[9, 2] <- cor(fcr_45$DP_TP,      bvr_45$DP_TP, use = 'complete.obs')
f45b45 <- ggplot(data = cor_pearson_f45_b45, aes(x = variable, y = pearson_r, fill = pearson_r > 0.5)) + geom_bar(stat = 'identity') +
  theme(legend.position = 'none',
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 35),
        strip.text = element_text(size = 35),
        plot.title = element_text(size = 40)) +
  ggtitle('F45 and B45')+
  scale_fill_manual('Above 0.5', values = c('gray45', 'coral')) 

#################
# f45 and b01
cor_pearson_f45_b01 <- data.frame('variable' = c('TN', 'NO3', 'NH4', 'TP', 'SRP', 'Chl-a', 'TNTP', 'DNTN', 'DPTP'),
                                  'pearson_r' = rep(NA))


bvr_01 <- data[data$res_site=='BVR_1',]
fcr_45 <- data_fcr[data_fcr$Site=='45',]
cor_pearson_f45_b01[6, 2] <- cor(fcr_45$Chla_ugL,   bvr_01$Chla_ugL)
cor_pearson_f45_b01[5, 2] <- cor(fcr_45$SRP_ugL,    bvr_01$SRP_ugL)
cor_pearson_f45_b01[2, 2] <- cor(fcr_45$NO3NO2_ugL, bvr_01$NO3NO2_ugL)
cor_pearson_f45_b01[3, 2] <- cor(fcr_45$NH4_ugL,    bvr_01$NH4_ugL, use = 'complete.obs')
cor_pearson_f45_b01[1, 2] <- cor(fcr_45$TN_ugL,     bvr_01$TN_ugL, use = 'complete.obs')
cor_pearson_f45_b01[4, 2] <- cor(fcr_45$TP_ugL,     bvr_01$TP_ugL, use = 'complete.obs')
cor_pearson_f45_b01[7, 2] <- cor(fcr_45$TN_TP,      bvr_01$TN_TP, use = 'complete.obs')
cor_pearson_f45_b01[8, 2] <- cor(fcr_45$DN_TN,      bvr_01$DN_TN, use = 'complete.obs')
cor_pearson_f45_b01[9, 2] <- cor(fcr_45$DP_TP,      bvr_01$DP_TP, use = 'complete.obs')
f45b01 <- ggplot(data = cor_pearson_f45_b01, aes(x = variable, y = pearson_r, fill = pearson_r > 0.5)) + geom_bar(stat = 'identity') +
  theme(legend.position = 'none',
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 35),
        strip.text = element_text(size = 35),
        plot.title = element_text(size = 40)) +
  ggtitle('F45 and B01')+
  scale_fill_manual('Above 0.5', values = c('gray45', 'coral')) 

#################
# f50 and b50
cor_pearson_f50_b50 <- data.frame('variable' = c('TN', 'NO3', 'NH4', 'TP', 'SRP', 'Chl-a', 'TNTP', 'DNTN', 'DPTP'),
                                  'pearson_r' = rep(NA))


bvr_50 <- data_bvr[data_bvr$Site=='50',]
fcr_50 <- data_fcr[data_fcr$Site=='50',]
cor_pearson_f50_b50[6, 2] <- cor(fcr_50$Chla_ugL,   bvr_50$Chla_ugL)
cor_pearson_f50_b50[5, 2] <- cor(fcr_50$SRP_ugL,    bvr_50$SRP_ugL)
cor_pearson_f50_b50[2, 2] <- cor(fcr_50$NO3NO2_ugL, bvr_50$NO3NO2_ugL)
cor_pearson_f50_b50[3, 2] <- cor(fcr_50$NH4_ugL,    bvr_50$NH4_ugL, use = 'complete.obs')
cor_pearson_f50_b50[1, 2] <- cor(fcr_50$TN_ugL,     bvr_50$TN_ugL, use = 'complete.obs')
cor_pearson_f50_b50[4, 2] <- cor(fcr_50$TP_ugL,     bvr_50$TP_ugL, use = 'complete.obs')
cor_pearson_f50_b50[7, 2] <- cor(fcr_50$TN_TP,      bvr_50$TN_TP, use = 'complete.obs')
cor_pearson_f50_b50[8, 2] <- cor(fcr_50$DN_TN,      bvr_50$DN_TN, use = 'complete.obs')
cor_pearson_f50_b50[9, 2] <- cor(fcr_50$DP_TP,      bvr_50$DP_TP, use = 'complete.obs')
f50b50 <- ggplot(data = cor_pearson_f50_b50, aes(x = variable, y = pearson_r, fill = pearson_r > 0.5)) + geom_bar(stat = 'identity') +
  theme(axis.text = element_text(size = 35),
        axis.title = element_text(size = 35),
        strip.text = element_text(size = 35),
        plot.title = element_text(size = 40)) +
  ggtitle('F50 and B50')+
  scale_fill_manual('Above 0.5', values = c('gray45', 'coral')) 

png("./Figures/reservoir continuum/Pearsons_by_site.png", width = 1800, height = 1100)
(f20b20 + f20b30 + f30b20 )/(f30b30 + f45b45 + f45b01)
dev.off()

png("./Figures/reservoir continuum/Pearsons_Site50.png", width = 1200, height = 1100)
f50b50
dev.off()


#########################################################################################################################################
#####################################################################################
# calculate synchrony between FCR and BVR 
# use SPEARMAN correlation instead
data_bvr <- data[data$Reservoir=='BVR' & data$Site %in% in_reservoir,]
data_fcr <- data[data$Reservoir=='FCR' & data$Site %in% in_reservoir,]
cor_spearman <- data.frame('variable' = c('TN', 'NO3', 'NH4', 'TP', 'SRP', 'Chl-a', 'TNTP', 'DNTN', 'DPTP'),
                          'spearman_r' = rep(NA))

# correlations between all sites at BVR vs FCR
cor_spearman[6, 2] <- cor(data_fcr$Chla_ugL, data_bvr$Chla_ugL, method = 'spearman')
cor_spearman[5, 2] <- cor(data_fcr$SRP_ugL, data_bvr$SRP_ugL, method = 'spearman')
cor_spearman[2, 2] <- cor(data_fcr$NO3NO2_ugL, data_bvr$NO3NO2_ugL, method = 'spearman')
cor_spearman[3, 2] <- cor(data_fcr$NH4_ugL, data_bvr$NH4_ugL, use = 'complete.obs', method = 'spearman')
cor_spearman[1, 2] <- cor(data_fcr$TN_ugL, data_bvr$TN_ugL, use = 'complete.obs', method = 'spearman')
cor_spearman[4, 2] <- cor(data_fcr$TP_ugL, data_bvr$TP_ugL, use = 'complete.obs', method = 'spearman')
cor_spearman[7, 2] <- cor(data_fcr$TN_TP, data_bvr$TN_TP, use = 'complete.obs', method = 'spearman')
cor_spearman[8, 2] <- cor(data_fcr$DN_TN, data_bvr$DN_TN, use = 'complete.obs', method = 'spearman')
cor_spearman[9, 2] <- cor(data_fcr$DP_TP, data_bvr$DP_TP, use = 'complete.obs', method = 'spearman')

png("./Figures/reservoir continuum/Spearman_r.png", width = 1200, height = 1100)
ggplot(data = cor_spearman, aes(x = variable, y = spearman_r, fill = spearman_r > 0.5)) + geom_bar(stat = 'identity') +
  theme(axis.text = element_text(size = 35),
        axis.title = element_text(size = 35),
        strip.text = element_text(size = 35)) +
  scale_fill_manual('Above 0.5', values = c('gray45', 'cornflowerblue')) 
dev.off()

# now do this individually for each site pairing
#########################
# f20 and b20
cor_spearman_f20_b20 <- data.frame('variable' = c('TN', 'NO3', 'NH4', 'TP', 'SRP', 'Chl-a', 'TNTP', 'DNTN', 'DPTP'),
                                  'spearman_r' = rep(NA))


bvr_20 <- data_bvr[data_bvr$Site=='20',]
fcr_20 <- data_fcr[data_fcr$Site=='20',]
cor_spearman_f20_b20[6, 2] <- cor(fcr_20$Chla_ugL,   bvr_20$Chla_ugL, method = 'spearman')
cor_spearman_f20_b20[5, 2] <- cor(fcr_20$SRP_ugL,    bvr_20$SRP_ugL, method = 'spearman')
cor_spearman_f20_b20[2, 2] <- cor(fcr_20$NO3NO2_ugL, bvr_20$NO3NO2_ugL, method = 'spearman')
cor_spearman_f20_b20[3, 2] <- cor(fcr_20$NH4_ugL,    bvr_20$NH4_ugL, use = 'complete.obs', method = 'spearman')
cor_spearman_f20_b20[1, 2] <- cor(fcr_20$TN_ugL,     bvr_20$TN_ugL, use = 'complete.obs', method = 'spearman')
cor_spearman_f20_b20[4, 2] <- cor(fcr_20$TP_ugL,     bvr_20$TP_ugL, use = 'complete.obs', method = 'spearman')
cor_spearman_f20_b20[7, 2] <- cor(fcr_20$TN_TP,      bvr_20$TN_TP, use = 'complete.obs', method = 'spearman')
cor_spearman_f20_b20[8, 2] <- cor(fcr_20$DN_TN,      bvr_20$DN_TN, use = 'complete.obs', method = 'spearman')
cor_spearman_f20_b20[9, 2] <- cor(fcr_20$DP_TP,      bvr_20$DP_TP, use = 'complete.obs', method = 'spearman')
f20b20 <- ggplot(data = cor_spearman_f20_b20, aes(x = variable, y = spearman_r, fill = spearman_r > 0.5)) + geom_bar(stat = 'identity') +
  theme(legend.position = 'none',
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 35),
        strip.text = element_text(size = 35),
        plot.title = element_text(size = 40)) +
  ggtitle('F20 and B20')+
  scale_fill_manual('Above 0.5', values = c('gray45', 'cornflowerblue')) 

#################
# f20 and b30
cor_spearman_f20_b30 <- data.frame('variable' = c('TN', 'NO3', 'NH4', 'TP', 'SRP', 'Chl-a', 'TNTP', 'DNTN', 'DPTP'),
                                  'pearson_r' = rep(NA))


bvr_30 <- data_bvr[data_bvr$Site=='30',]
fcr_20 <- data_fcr[data_fcr$Site=='20',]
cor_spearman_f20_b30[6, 2] <- cor(fcr_20$Chla_ugL,   bvr_30$Chla_ugL, method = 'spearman')
cor_spearman_f20_b30[5, 2] <- cor(fcr_20$SRP_ugL,    bvr_30$SRP_ugL, method = 'spearman')
cor_spearman_f20_b30[2, 2] <- cor(fcr_20$NO3NO2_ugL, bvr_30$NO3NO2_ugL, method = 'spearman')
cor_spearman_f20_b30[3, 2] <- cor(fcr_20$NH4_ugL,    bvr_30$NH4_ugL, use = 'complete.obs', method = 'spearman')
cor_spearman_f20_b30[1, 2] <- cor(fcr_20$TN_ugL,     bvr_30$TN_ugL, use = 'complete.obs', method = 'spearman')
cor_spearman_f20_b30[4, 2] <- cor(fcr_20$TP_ugL,     bvr_30$TP_ugL, use = 'complete.obs', method = 'spearman')
cor_spearman_f20_b30[7, 2] <- cor(fcr_20$TN_TP,      bvr_30$TN_TP, use = 'complete.obs', method = 'spearman')
cor_spearman_f20_b30[8, 2] <- cor(fcr_20$DN_TN,      bvr_30$DN_TN, use = 'complete.obs', method = 'spearman')
cor_spearman_f20_b30[9, 2] <- cor(fcr_20$DP_TP,      bvr_30$DP_TP, use = 'complete.obs', method = 'spearman')
f20b30 <- ggplot(data = cor_spearman_f20_b30, aes(x = variable, y = spearman_r, fill = spearman_r > 0.5)) + geom_bar(stat = 'identity') +
  theme(legend.position = 'none', 
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 35),
        strip.text = element_text(size = 35),
        plot.title = element_text(size = 40)) +
  ggtitle('F20 and B30')+
  scale_fill_manual('Above 0.5', values = c('gray45', 'cornflowerblue')) 

#################
# f20 and b30
cor_spearman_b30_f30 <- data.frame('variable' = c('TN', 'NO3', 'NH4', 'TP', 'SRP', 'Chl-a', 'TNTP', 'DNTN', 'DPTP'),
                                  'spearman_r' = rep(NA))


bvr_30 <- data_bvr[data_bvr$Site=='30',]
fcr_20 <- data_fcr[data_fcr$Site=='20',]
cor_spearman_f20_b30[6, 2] <- cor(fcr_20$Chla_ugL,   bvr_30$Chla_ugL, method = 'spearman')
cor_spearman_f20_b30[5, 2] <- cor(fcr_20$SRP_ugL,    bvr_30$SRP_ugL, method = 'spearman')
cor_spearman_f20_b30[2, 2] <- cor(fcr_20$NO3NO2_ugL, bvr_30$NO3NO2_ugL, method = 'spearman')
cor_spearman_f20_b30[3, 2] <- cor(fcr_20$NH4_ugL,    bvr_30$NH4_ugL, use = 'complete.obs', method = 'spearman')
cor_spearman_f20_b30[1, 2] <- cor(fcr_20$TN_ugL,     bvr_30$TN_ugL, use = 'complete.obs', method = 'spearman')
cor_spearman_f20_b30[4, 2] <- cor(fcr_20$TP_ugL,     bvr_30$TP_ugL, use = 'complete.obs', method = 'spearman')
cor_spearman_f20_b30[7, 2] <- cor(fcr_20$TN_TP,      bvr_30$TN_TP, use = 'complete.obs', method = 'spearman')
cor_spearman_f20_b30[8, 2] <- cor(fcr_20$DN_TN,      bvr_30$DN_TN, use = 'complete.obs', method = 'spearman')
cor_spearman_f20_b30[9, 2] <- cor(fcr_20$DP_TP,      bvr_30$DP_TP, use = 'complete.obs', method = 'spearman')
f20b30 <- ggplot(data = cor_spearman_f20_b30, aes(x = variable, y = spearman_r, fill = spearman_r > 0.5)) + geom_bar(stat = 'identity') +
  theme(legend.position = 'none',
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 35),
        strip.text = element_text(size = 35),
        plot.title = element_text(size = 40)) +
  ggtitle('F20 and B30')+
  scale_fill_manual('Above 0.5', values = c('gray45', 'cornflowerblue')) 

#################
# f30 and b20
cor_spearman_f30_b20 <- data.frame('variable' = c('TN', 'NO3', 'NH4', 'TP', 'SRP', 'Chl-a', 'TNTP', 'DNTN', 'DPTP'),
                                  'spearman_r' = rep(NA))


bvr_20 <- data_bvr[data_bvr$Site=='20',]
fcr_30 <- data_fcr[data_fcr$Site=='30',]
cor_spearman_f30_b20[6, 2] <- cor(fcr_30$Chla_ugL,   bvr_20$Chla_ugL, method = 'spearman')
cor_spearman_f30_b20[5, 2] <- cor(fcr_30$SRP_ugL,    bvr_20$SRP_ugL, method = 'spearman')
cor_spearman_f30_b20[2, 2] <- cor(fcr_30$NO3NO2_ugL, bvr_20$NO3NO2_ugL, method = 'spearman')
cor_spearman_f30_b20[3, 2] <- cor(fcr_30$NH4_ugL,    bvr_20$NH4_ugL, use = 'complete.obs', method = 'spearman')
cor_spearman_f30_b20[1, 2] <- cor(fcr_30$TN_ugL,     bvr_20$TN_ugL, use = 'complete.obs', method = 'spearman')
cor_spearman_f30_b20[4, 2] <- cor(fcr_30$TP_ugL,     bvr_20$TP_ugL, use = 'complete.obs', method = 'spearman')
cor_spearman_f30_b20[7, 2] <- cor(fcr_30$TN_TP,      bvr_20$TN_TP, use = 'complete.obs', method = 'spearman')
cor_spearman_f30_b20[8, 2] <- cor(fcr_30$DN_TN,      bvr_20$DN_TN, use = 'complete.obs', method = 'spearman')
cor_spearman_f30_b20[9, 2] <- cor(fcr_30$DP_TP,      bvr_20$DP_TP, use = 'complete.obs', method = 'spearman')
f30b20 <- ggplot(data = cor_spearman_f30_b20, aes(x = variable, y = spearman_r, fill = spearman_r > 0.5)) + geom_bar(stat = 'identity') +
  theme(legend.position = 'none',
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 35),
        strip.text = element_text(size = 35),
        plot.title = element_text(size = 40)) +
  ggtitle('F30 and B20')+
  scale_fill_manual('Above 0.5', values = c('gray45', 'cornflowerblue')) 

#################
# f30 and b30
cor_spearman_f30_b30 <- data.frame('variable' = c('TN', 'NO3', 'NH4', 'TP', 'SRP', 'Chl-a', 'TNTP', 'DNTN', 'DPTP'),
                                  'spearman_r' = rep(NA))


bvr_30 <- data_bvr[data_bvr$Site=='30',]
fcr_30 <- data_fcr[data_fcr$Site=='30',]
cor_spearman_f30_b30[6, 2] <- cor(fcr_30$Chla_ugL,   bvr_30$Chla_ugL, method = 'spearman')
cor_spearman_f30_b30[5, 2] <- cor(fcr_30$SRP_ugL,    bvr_30$SRP_ugL, method = 'spearman')
cor_spearman_f30_b30[2, 2] <- cor(fcr_30$NO3NO2_ugL, bvr_30$NO3NO2_ugL, method = 'spearman')
cor_spearman_f30_b30[3, 2] <- cor(fcr_30$NH4_ugL,    bvr_30$NH4_ugL, use = 'complete.obs', method = 'spearman')
cor_spearman_f30_b30[1, 2] <- cor(fcr_30$TN_ugL,     bvr_30$TN_ugL, use = 'complete.obs', method = 'spearman')
cor_spearman_f30_b30[4, 2] <- cor(fcr_30$TP_ugL,     bvr_30$TP_ugL, use = 'complete.obs', method = 'spearman')
cor_spearman_f30_b30[7, 2] <- cor(fcr_30$TN_TP,      bvr_30$TN_TP, use = 'complete.obs', method = 'spearman')
cor_spearman_f30_b30[8, 2] <- cor(fcr_30$DN_TN,      bvr_30$DN_TN, use = 'complete.obs', method = 'spearman')
cor_spearman_f30_b30[9, 2] <- cor(fcr_30$DP_TP,      bvr_30$DP_TP, use = 'complete.obs', method = 'spearman')
f30b30 <- ggplot(data = cor_spearman_f30_b30, aes(x = variable, y = spearman_r, fill = spearman_r > 0.5)) + geom_bar(stat = 'identity') +
  theme(legend.position = 'none',
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 35),
        strip.text = element_text(size = 35),
        plot.title = element_text(size = 40)) +
  ggtitle('F30 and B30')+
  scale_fill_manual('Above 0.5', values = c('gray45', 'cornflowerblue')) 

#################
# f45 and b45
cor_spearman_f45_b45 <- data.frame('variable' = c('TN', 'NO3', 'NH4', 'TP', 'SRP', 'Chl-a', 'TNTP', 'DNTN', 'DPTP'),
                                  'spearman_r' = rep(NA))


bvr_45 <- data_bvr[data_bvr$Site=='45',]
fcr_45 <- data_fcr[data_fcr$Site=='45',]
cor_spearman_f45_b45[6, 2] <- cor(fcr_45$Chla_ugL,   bvr_45$Chla_ugL, method = 'spearman')
cor_spearman_f45_b45[5, 2] <- cor(fcr_45$SRP_ugL,    bvr_45$SRP_ugL, method = 'spearman')
cor_spearman_f45_b45[2, 2] <- cor(fcr_45$NO3NO2_ugL, bvr_45$NO3NO2_ugL, method = 'spearman') # sd of BVR NO3 is zero, all values are 3 ug/L
cor_spearman_f45_b45[3, 2] <- cor(fcr_45$NH4_ugL,    bvr_45$NH4_ugL, use = 'complete.obs', method = 'spearman')
cor_spearman_f45_b45[1, 2] <- cor(fcr_45$TN_ugL,     bvr_45$TN_ugL, use = 'complete.obs', method = 'spearman')
cor_spearman_f45_b45[4, 2] <- cor(fcr_45$TP_ugL,     bvr_45$TP_ugL, use = 'complete.obs', method = 'spearman')
cor_spearman_f45_b45[7, 2] <- cor(fcr_45$TN_TP,      bvr_45$TN_TP, use = 'complete.obs', method = 'spearman')
cor_spearman_f45_b45[8, 2] <- cor(fcr_45$DN_TN,      bvr_45$DN_TN, use = 'complete.obs', method = 'spearman')
cor_spearman_f45_b45[9, 2] <- cor(fcr_45$DP_TP,      bvr_45$DP_TP, use = 'complete.obs', method = 'spearman')
f45b45 <- ggplot(data = cor_spearman_f45_b45, aes(x = variable, y = spearman_r, fill = spearman_r > 0.5)) + geom_bar(stat = 'identity') +
  theme(legend.position = 'none',
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 35),
        strip.text = element_text(size = 35),
        plot.title = element_text(size = 40)) +
  ggtitle('F45 and B45')+
  scale_fill_manual('Above 0.5', values = c('gray45', 'cornflowerblue')) 

#################
# f45 and b01
cor_spearman_f45_b01 <- data.frame('variable' = c('TN', 'NO3', 'NH4', 'TP', 'SRP', 'Chl-a', 'TNTP', 'DNTN', 'DPTP'),
                                  'spearman_r' = rep(NA))


bvr_01 <- data[data$res_site=='BVR_1',]
fcr_45 <- data_fcr[data_fcr$Site=='45',]
cor_spearman_f45_b01[6, 2] <- cor(fcr_45$Chla_ugL,   bvr_01$Chla_ugL, method = 'spearman')
cor_spearman_f45_b01[5, 2] <- cor(fcr_45$SRP_ugL,    bvr_01$SRP_ugL, method = 'spearman')
cor_spearman_f45_b01[2, 2] <- cor(fcr_45$NO3NO2_ugL, bvr_01$NO3NO2_ugL, method = 'spearman')
cor_spearman_f45_b01[3, 2] <- cor(fcr_45$NH4_ugL,    bvr_01$NH4_ugL, use = 'complete.obs', method = 'spearman')
cor_spearman_f45_b01[1, 2] <- cor(fcr_45$TN_ugL,     bvr_01$TN_ugL, use = 'complete.obs', method = 'spearman')
cor_spearman_f45_b01[4, 2] <- cor(fcr_45$TP_ugL,     bvr_01$TP_ugL, use = 'complete.obs', method = 'spearman')
cor_spearman_f45_b01[7, 2] <- cor(fcr_45$TN_TP,      bvr_01$TN_TP, use = 'complete.obs', method = 'spearman')
cor_spearman_f45_b01[8, 2] <- cor(fcr_45$DN_TN,      bvr_01$DN_TN, use = 'complete.obs', method = 'spearman')
cor_spearman_f45_b01[9, 2] <- cor(fcr_45$DP_TP,      bvr_01$DP_TP, use = 'complete.obs', method = 'spearman')
f45b01 <- ggplot(data = cor_spearman_f45_b01, aes(x = variable, y = spearman_r, fill = spearman_r > 0.5)) + geom_bar(stat = 'identity') +
  theme(legend.position = 'none',
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 35),
        strip.text = element_text(size = 35),
        plot.title = element_text(size = 40)) +
  ggtitle('F45 and B01')+
  scale_fill_manual('Above 0.5', values = c('gray45', 'cornflowerblue')) 

#################
# f50 and b50
cor_spearman_f50_b50 <- data.frame('variable' = c('TN', 'NO3', 'NH4', 'TP', 'SRP', 'Chl-a', 'TNTP', 'DNTN', 'DPTP'),
                                  'spearman_r' = rep(NA))


bvr_50 <- data_bvr[data_bvr$Site=='50',]
fcr_50 <- data_fcr[data_fcr$Site=='50',]
cor_spearman_f50_b50[6, 2] <- cor(fcr_50$Chla_ugL,   bvr_50$Chla_ugL, method = 'spearman')
cor_spearman_f50_b50[5, 2] <- cor(fcr_50$SRP_ugL,    bvr_50$SRP_ugL, method = 'spearman')
cor_spearman_f50_b50[2, 2] <- cor(fcr_50$NO3NO2_ugL, bvr_50$NO3NO2_ugL, method = 'spearman')
cor_spearman_f50_b50[3, 2] <- cor(fcr_50$NH4_ugL,    bvr_50$NH4_ugL, use = 'complete.obs', method = 'spearman')
cor_spearman_f50_b50[1, 2] <- cor(fcr_50$TN_ugL,     bvr_50$TN_ugL, use = 'complete.obs', method = 'spearman')
cor_spearman_f50_b50[4, 2] <- cor(fcr_50$TP_ugL,     bvr_50$TP_ugL, use = 'complete.obs', method = 'spearman')
cor_spearman_f50_b50[7, 2] <- cor(fcr_50$TN_TP,      bvr_50$TN_TP, use = 'complete.obs', method = 'spearman')
cor_spearman_f50_b50[8, 2] <- cor(fcr_50$DN_TN,      bvr_50$DN_TN, use = 'complete.obs', method = 'spearman')
cor_spearman_f50_b50[9, 2] <- cor(fcr_50$DP_TP,      bvr_50$DP_TP, use = 'complete.obs', method = 'spearman')
f50b50 <- ggplot(data = cor_spearman_f50_b50, aes(x = variable, y = spearman_r, fill = spearman_r > 0.5)) + geom_bar(stat = 'identity') +
  theme(axis.text = element_text(size = 35),
        axis.title = element_text(size = 35),
        strip.text = element_text(size = 35),
        plot.title = element_text(size = 40)) +
  ggtitle('F50 and B50')+
  scale_fill_manual('Above 0.5', values = c('gray45', 'cornflowerblue')) 

png("./Figures/reservoir continuum/Spearman_by_site.png", width = 1800, height = 1100)
(f20b20 + f20b30 + f30b20 )/(f30b30 + f45b45 + f45b01)
dev.off()

png("./Figures/reservoir continuum/Spearman_Site50.png", width = 1200, height = 1100)
f50b50
dev.off()
