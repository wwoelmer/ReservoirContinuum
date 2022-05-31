# make time series figure

library(tidyverse)
library(patchwork)
library(lubridate)
library(EnvStats)
library(ggpubr)
library(ggpmisc)

r_col <- c('olivedrab3', 'royalblue1')


data <-read.csv('./Data/continuum_data.csv')
data$Date <- as.Date(data$Date)
data$distance_from_stream <- as.numeric(data$distance_from_stream)


long_data <- data %>%   
  select(Site, Reservoir, Date, distance_from_stream, distance_m, Flow_cms, TN_ugL:T) %>% 
  pivot_longer(TN_ugL:T, names_to = 'variable', values_to = 'value')

vars_keep <-  c('T', 'A', 
                'DOC_mgL',  'Chla_ugL','TN_ugL', 'TP_ugL',
                'NH4_ugL', 'NO3NO2_ugL', 'SRP_ugL') 

long <- long_data[long_data$variable %in% vars_keep,]

levels <- c('TN_ugL', 'TP_ugL', 'Chla_ugL',
            'NH4_ugL', 'NO3NO2_ugL', 'SRP_ugL',
            'DOC_mgL','T', 'A')

labels <- c('TN', 'TP', 'Chl-a', 
            'NH4', 'NO3', 'SRP',
            'DOC', 'T-autoch',  'A-alloch') 
names(labels) <- levels
long$variable <- factor(long$variable, levels = levels)
long$Month <- as.factor(month(long$Date))


range <- long %>% 
  filter(distance_from_stream > 0) %>% 
  group_by(variable, Month, Reservoir) %>% 
  mutate(range = round(abs(range(value, na.rm = TRUE)[1] - range(value, na.rm = TRUE)[2]), 4)) %>%
  distinct(Reservoir, Date, variable, .keep_all = TRUE) %>% 
  select(Reservoir, Date, variable, range)
ggplot(range, aes(x = Date, y = range, color = Reservoir)) +
  geom_line(size = 2) +
  facet_wrap(~variable,  scales = "free_y") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

######################################################################################################################
### time series color by date, one for each reservoir


## FCR
labels_f <- c('d) TN (μg/L)', 'e) TP (μg/L)', 'f) Chl-a (μg/L)',
            'j) NH4 (μg/L)', 'k) NO3 (μg/L)', 'l) SRP (μg/L)',
            'p) DOC (mg/L)', 'q) T-autoch (RFU)',  'r) A-alloch (RFU)') 
names(labels_f) <- levels
long$variable <- factor(long$variable, levels = levels)
long$Month <- as.factor(month(long$Date))


f <- ggplot(data = long[long$Reservoir=='FCR' & long$distance_from_stream > 0,], aes(x = distance_m, y = value, col = Month)) +
  geom_point(size = 2) +
  geom_smooth(aes(col = Month), se = FALSE) +
  xlab('Distance from stream (m)') +
  ylab('Concentration') +
  facet_wrap(~variable, scales = "free_y", labeller = labeller(variable = labels_f)) + 
  scale_color_manual(values = rev(hcl.colors(7, "Zissou 1"))) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  ggtitle('Falling Creek Reservoir')
# concentration units: add in figure caption?
f

## BVR
labels_b <- c('a) TN (μg/L)', 'b) TP (μg/L)', 'c) Chl-a (μg/L)',
              'g) NH4 (μg/L)', 'h) NO3 (μg/L)', 'i) SRP (μg/L)',
              'm) DOC (mg/L)', 'n) T-autoch (RFU)',  'o) A-alloch (RFU)') 
names(labels_b) <- levels
long$variable <- factor(long$variable, levels = levels)
long$Month <- as.factor(month(long$Date))
b <- ggplot(data = long[long$Reservoir=='BVR' & long$distance_from_stream > 0,], aes(x = distance_m, y = value, col = Month)) +
  geom_point(size = 2) +
  geom_smooth(aes(col = Month), se = FALSE) +
  xlab('Distance from stream (m)') +
  ylab('Concentration') +
  facet_wrap(~variable, scales = "free_y", labeller = labeller(variable = labels_b)) + 
  scale_color_manual(values = rev(hcl.colors(7, "Zissou 1"))) +
  theme_bw() +
  labs(col = 'Month') +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none") +
  ggtitle('Beaverdam Reservoir')
b
plot <- ggarrange(b, f, nrow = 1, ncol = 2, common.legend = TRUE, legend = 'right')
plot
ggsave('./Figures/Fig3_timeseries.png', plot)

############################################################################################################
### stoiciometry SI figure
# remove 07/22/2019 sampling date (replicate for 07/18/2019)
data <- data[data$Date!='2019-07-22', ]

long_stoich <- data %>%   
  select(Site, Reservoir, Date, distance_from_stream, distance_m, TN_TP:DN_DP) %>% 
  pivot_longer(TN_TP:DN_DP, names_to = 'variable', values_to = 'value')


long_stoich$Month <- as.factor(month(long_stoich$Date))
levels <- c('TN_TP', 'DN_DP', 'DP_TP', 'DN_TN')
labels <- c('TN:TP', 'DIN:SRP', 'SRP:TP', 'DIN:TN')
names(labels) <- levels
long_stoich$variable <- factor(long_stoich$variable, levels = levels)

# calculate mean and sd of each ratio
summary_stoich <- long_stoich %>% 
  filter(distance_from_stream > 0) %>% 
  group_by(variable) %>% 
  mutate(mean = mean(value, na.rm = TRUE)) %>% 
  mutate(sd = sd(value, na.rm = TRUE)) %>% 
  group_by(variable, Site, Reservoir) %>% 
  mutate(mean_site = mean(value, na.rm = TRUE)) %>% 
  mutate(sd_site = sd(value, na.rm = TRUE)) %>% 
  distinct(Reservoir, Site, variable, .keep_all = TRUE)




## FCR
fcr <- long_stoich[long_stoich$Reservoir=='FCR' & long_stoich$distance_from_stream > 0,]
fs <- ggplot(data = long_stoich[long_stoich$Reservoir=='FCR' & long_stoich$distance_from_stream > 0,], aes(x = distance_m, y = value, col = Month)) +
  geom_point(size = 2) +
  geom_smooth(aes(col = Month), se = FALSE) +
  xlab('Distance from stream (m)') +
  ylab('Ratio') +
  facet_wrap(~variable, scales = "free_y", labeller = labeller(variable = labels)) + 
  scale_color_manual(values = rev(hcl.colors(7, "Zissou 1"))) +
  theme_bw() +
  labs(col = 'Month') +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none") +
  ggtitle('Falling Creek Reservoir')
fs
## BVR
bvr <- long_stoich[long_stoich$Reservoir=='BVR' & long_stoich$distance_from_stream > 0 & long_stoich$variable=='DN_DP',]
bs <- ggplot(data = long_stoich[long_stoich$Reservoir=='BVR' & long_stoich$distance_from_stream > 0,], aes(x = distance_m, y = value, col = Month)) +
  geom_point(size = 2) +
  geom_smooth(aes(col = Month), se = FALSE) +
  xlab('Distance from stream (m)') +
  ylab('Ratio') +
  facet_wrap(~variable, scales = "free_y", labeller = labeller(variable = labels)) + 
  scale_color_manual(values = rev(hcl.colors(7, "Zissou 1"))) +
  theme_bw() +
  labs(col = 'Month') +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none") +
  ggtitle('Beaverdam Reservoir')

bs
plot3 <- ggarrange(bs, fs, nrow = 1, ncol = 2, common.legend = TRUE, legend = 'right') 
ggsave('./Figures/SIFig_stoich_timeseries.png', plot3)



