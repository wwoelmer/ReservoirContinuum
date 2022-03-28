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


# some SRP values are 0, so add very very small value to these for dividing by this value later
#min_val <- min(data[data$SRP_ugL>0,"SRP_ugL"])
#data$SRP_ugL <- data$SRP_ugL + min_val

long <- data %>%   
  select(Site, Reservoir, Date, distance_from_stream, distance_m, Flow_cms, TN_ugL:T) %>% 
  pivot_longer(TN_ugL:T, names_to = 'variable', values_to = 'value')

vars_keep <-  c('T', 'A', 
                'DOC_mgL',  'Chla_ugL','TN_ugL', 'TP_ugL',
                'NH4_ugL', 'NO3NO2_ugL', 'SRP_ugL') 
vars_stoich <-  c('TN_TP', 'DN_DP', 'DP_TP', 'DN_TN')
stoich <- long[long$variable %in% vars_stoich,]

long <- long[long$variable %in% vars_keep,]

levels <- c('NH4_ugL', 'NO3NO2_ugL', 'SRP_ugL',
            'DOC_mgL',  'TN_ugL', 'TP_ugL',
            'Chla_ugL','T', 'A')

labels <- c('NH4', 'NO3', 'SRP',
            'DOC', 'TN', 'TP',
            'Chl-a', 'T-autoch',  'A-alloch') 
names(labels) <- levels
long$variable <- factor(long$variable, levels = levels)
long$Month <- as.factor(month(long$Date))


range <- long %>% 
  filter(distance_from_stream > 0) %>% 
  group_by(variable, Month, Reservoir) %>% 
  mutate(range = round(abs(range(value, na.rm = TRUE)[1] - range(value, na.rm = TRUE)[2]), 4)) %>% 
  distinct(Reservoir, Date, variable, .keep_all = TRUE)

######################################################################################################################
### time series color by date, one for each reservoir

## FCR
labels_f <- c('d) NH4 (μg/L)', 'e) NO3 (μg/L)', 'f) SRP (μg/L)',
            'j) DOC (mg/L)', 'k) TN (μg/L)', 'l) TP (μg/L)',
            'p) Chl-a (μg/L)', 'q) T-autoch (RFU)',  'r) A-alloch (RFU)') 
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
labels_b <- c('a) NH4 (μg/L)', 'b) NO3 (μg/L)', 'c) SRP (μg/L)',
              'g) DOC (mg/L)', 'h) TN (μg/L)', 'i) TP (μg/L)',
              'm) Chl-a (μg/L)', 'n) T-autoch (RFU)',  'o) A-alloch (RFU)') 
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
ggsave('./Figures/Fig3_timeseries.png', plot)

#################################################################################################################
# make Thornton figure with observed data
thorn <- data %>% 
  select(Date:Site, distance_from_stream, TN_ugL:SRP_ugL, Chla_ugL) %>% 
  filter(res_site != 'FCR_1' & res_site != 'FCR_100' & res_site != 'FCR_101' & res_site != 'FCR_102') %>% 
  filter(res_site != 'FCR_99' & res_site != 'FCR_200' & res_site != 'BVR_100' & res_site != 'BVR_200')

thorn <- thorn %>% 
  group_by(Reservoir, distance_from_stream) %>% 
  mutate(total_n = mean(TN_ugL, na.rm = TRUE)) %>% 
  mutate(total_p = mean(TP_ugL, na.rm = TRUE)) %>% 
  mutate(soluble_p = mean(SRP_ugL, na.rm = TRUE)) %>% 
  mutate(soluble_n = mean((NH4_ugL + NO3NO2_ugL), na.rm = TRUE)) %>% 
  mutate(chla = mean(Chla_ugL, na.rm = TRUE)) %>% 
  mutate(sd_total_n = sd(TN_ugL, na.rm = TRUE)) %>% 
  mutate(sd_total_p = sd(TP_ugL, na.rm = TRUE)) %>% 
  mutate(sd_soluble_p = sd(SRP_ugL, na.rm = TRUE)) %>% 
  mutate(sd_soluble_n = sd((NH4_ugL + NO3NO2_ugL), na.rm = TRUE)) %>% 
  mutate(sd_chla = sd(Chla_ugL, na.rm = TRUE)) %>% 
  select(res_site, Reservoir, Site, distance_from_stream, total_n:sd_chla) %>% 
  distinct(res_site, .keep_all = TRUE)

n <- ggplot(thorn, aes(x = distance_from_stream, y = total_n, color = Reservoir)) +
  geom_point(size = 2) +
  geom_line(aes(color = Reservoir), size = 2) +
  geom_ribbon(data = thorn[thorn$Reservoir=='FCR', ], aes(ymin = total_n - sd_total_n, ymax = total_n + sd_total_n, fill = Reservoir), alpha = 0.2) +
  geom_ribbon(data = thorn[thorn$Reservoir=='BVR', ], aes(ymin = total_n - sd_total_n, ymax = total_n + sd_total_n, fill = Reservoir), alpha = 0.2) +
  xlab('Distance from upstream') +
  ylab('Concentration') +
  scale_x_continuous(breaks = c(1, 2, 3)) +
  scale_color_manual(values = r_col) +
  scale_fill_manual(values = r_col) +
  theme_bw() +
  labs(col = 'Reservoir') +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  ggtitle('Total Nitrogen')

p <- ggplot(thorn, aes(x = distance_from_stream, y = total_p, color = Reservoir)) +
  geom_point(size = 2) +
  geom_line(aes(color = Reservoir), size = 2) +
  geom_ribbon(data = thorn[thorn$Reservoir=='FCR', ], aes(ymin = total_p - sd_total_p, ymax = total_p + sd_total_p, fill = Reservoir), alpha = 0.2) +
  geom_ribbon(data = thorn[thorn$Reservoir=='BVR', ], aes(ymin = total_p - sd_total_p, ymax = total_p + sd_total_p, fill = Reservoir), alpha = 0.2) +
  xlab('Distance from upstream') +
  ylab('Concentration') +
  scale_x_continuous(breaks = c(1, 2, 3)) +
  scale_color_manual(values = r_col) +
  scale_fill_manual(values = r_col) +
  theme_bw() +
  labs(col = 'Reservoir') +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())  +
  ggtitle('Total Phosphorus')


sp <- ggplot(thorn, aes(x = distance_from_stream, y = soluble_p, color = Reservoir)) +
  #geom_point(size = 2) +
  geom_line(aes(color = Reservoir), size = 2, linetype = 'longdash') +
  geom_ribbon(data = thorn[thorn$Reservoir=='FCR', ], aes(ymin = soluble_p - sd_soluble_p, ymax = soluble_p + sd_soluble_p, fill = Reservoir), alpha = 0.2) +
  geom_ribbon(data = thorn[thorn$Reservoir=='BVR', ], aes(ymin = soluble_p - sd_soluble_p, ymax = soluble_p + sd_soluble_p, fill = Reservoir), alpha = 0.2) +
  xlab('Distance from upstream') +
  ylab('Concentration') +
  scale_x_continuous(breaks = c(1, 2, 3)) +
  scale_color_manual(values = r_col) +
  scale_fill_manual(values = r_col) +
  theme_bw() +
  labs(col = 'Reservoir') +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  ggtitle('Soluble Phosphorus')

sn <- ggplot(thorn, aes(x = distance_from_stream, y = soluble_n, color = Reservoir)) +
  #geom_point(size = 2) +
  geom_line(aes(color = Reservoir), size = 2, linetype = 'longdash') +
  geom_ribbon(data = thorn[thorn$Reservoir=='FCR', ], aes(ymin = soluble_n - sd_soluble_n, ymax = soluble_n + sd_soluble_n, fill = Reservoir), alpha = 0.2) +
  geom_ribbon(data = thorn[thorn$Reservoir=='BVR', ], aes(ymin = soluble_n - sd_soluble_n, ymax = soluble_n + sd_soluble_n, fill = Reservoir), alpha = 0.2) +
  xlab('Distance from upstream') +
  ylab('Concentration') +
  scale_x_continuous(breaks = c(1, 2, 3)) +
  scale_color_manual(values = r_col) +
  scale_fill_manual(values = r_col) +
  theme_bw() +
  labs(col = 'Reservoir') +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  ggtitle('Soluble Nitrogen')

chl <- ggplot(thorn, aes(x = distance_from_stream, y = chla, color = Reservoir)) +
  #geom_point(size = 2) +
  geom_line(aes(color = Reservoir), size = 2) +
  geom_ribbon(data = thorn[thorn$Reservoir=='FCR', ], aes(ymin = chla - sd_chla, ymax = chla + sd_chla, fill = Reservoir), alpha = 0.2) +
  geom_ribbon(data = thorn[thorn$Reservoir=='BVR', ], aes(ymin = chla - sd_chla, ymax = chla + sd_chla, fill = Reservoir), alpha = 0.2) +
  xlab('Distance from upstream') +
  ylab('Concentration') +
  scale_x_continuous(breaks = c(1, 2, 3)) +
  scale_color_manual(values = r_col) +
  scale_fill_manual(values = r_col) +
  theme_bw() +
  labs(col = 'Reservoir') +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  ggtitle('Chlorophyll-a')


plot2 <- ggarrange(n, p, chl, sn, sp, ncol = 3, nrow = 2, common.legend = TRUE)
ggsave('./Figures/Fig7_TNTPchl_thornton.png', plot2, width = 7, height = 4.6)



############################################################################################################
### stoiciometry figure

stoich$Month <- as.factor(month(stoich$Date))
levels <- c('TN_TP', 'DN_DP', 'DP_TP', 'DN_TN')
labels <- c('TN_TP', 'DN_DP', 'DP_TP', 'DN_TN')
names(labels) <- levels
stoich$variable <- factor(stoich$variable, levels = levels)



## FCR
fs <- ggplot(data = stoich[stoich$Reservoir=='FCR' & stoich$distance_from_stream > 0,], aes(x = distance_m, y = value, col = Month)) +
  geom_point(size = 2) +
  geom_smooth(aes(col = Month), se = FALSE) +
  xlab('Distance from stream (m)') +
  ylab('Concentration') +
  facet_wrap(~variable, scales = "free_y") + 
  scale_color_manual(values = rev(hcl.colors(7, "Zissou 1"))) +
  theme_bw() +
  labs(col = 'Month') +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none") +
  ggtitle('Falling Creek Reservoir')

## BVR
bvr <- stoich[stoich$Reservoir=='BVR' & stoich$distance_from_stream > 0 & stoich$variable=='DN_DP',]
bs <- ggplot(data = stoich[stoich$Reservoir=='BVR' & stoich$distance_from_stream > 0,], aes(x = distance_m, y = value, col = Month)) +
  geom_point(size = 2) +
  geom_smooth(aes(col = Month), se = FALSE) +
  xlab('Distance from stream (m)') +
  ylab('Concentration') +
  facet_wrap(~variable, scales = "free_y") + 
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









######################################################################################################################
### group all data over distance from stream

# without stream data
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
weir_stream <- c('102', '101', '100', '01')
ggplot(data = long[!(long$Site %in% weir_stream),], aes(x = distance_m, y = value)) +
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
