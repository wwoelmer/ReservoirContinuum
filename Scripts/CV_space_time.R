# boxplots of variables across time and space

library(tidyverse)
library(patchwork)
library(lubridate)
library(EnvStats)
library(ggpubr)
library(ggpmisc)
library(ggsignif)

r_col <- c('olivedrab3', 'royalblue1')


data <-read.csv('./Data/continuum_data.csv')

long <- data %>%   
  select(Site, Reservoir, Date, distance_from_stream, distance_m, Flow_cms, TN_ugL:T) %>% 
  pivot_longer(TN_ugL:T, names_to = 'variable', values_to = 'value')



#########################################################################################################################
#### plot CV as boxplot

# calculate heterogeneity within the reservoir (coefficient of variation) for each reservoir on each RC day

CV_time <- data[data$distance_from_stream>0,] %>% group_by(Reservoir, Site) %>% 
  mutate(CV_chl = cv(Chla_ugL, na.rm = TRUE)) %>% 
  mutate(CV_SRP = cv(SRP_ugL, na.rm = TRUE)) %>% 
  mutate(CV_NO3 = cv(NO3NO2_ugL, na.rm = TRUE)) %>% 
  mutate(CV_NH4 = cv(NH4_ugL, na.rm = TRUE)) %>% 
  mutate(CV_TP = cv(TP_ugL, na.rm = TRUE)) %>% 
  mutate(CV_TN = cv(TN_ugL, na.rm = TRUE)) %>% 
  mutate(CV_TNTP = cv(TN_TP, na.rm = TRUE)) %>% 
  mutate(CV_A = cv(A, na.rm = TRUE)) %>% 
  mutate(CV_T = cv(T, na.rm = TRUE)) %>% 
  mutate(CV_DOC = cv(DOC_mgL, na.rm = TRUE)) %>% 
  mutate(CV_DPTP = cv(DP_TP, na.rm = TRUE)) %>% 
  mutate(CV_DNTN = cv(DN_TN, na.rm = TRUE)) %>% 
  select(Date, Reservoir, distance_from_stream, distance_m, CV_chl:CV_DNTN) %>% 
  distinct(Site, Reservoir, .keep_all = TRUE)
CV_time$Date <- as.Date(CV_time$Date)

time_long <- CV_time %>% 
  pivot_longer(CV_chl:CV_DNTN, names_to = 'variable', values_to = 'cv')
time_long$axis <- 'time'

levels <- c('CV_T', 'CV_A',
            'CV_DOC',
            'CV_NH4', 'CV_NO3', 'CV_SRP',
            'CV_TN', 'CV_TP', 'CV_chl',
            'CV_TNTP', 'CV_DPTP', 'CV_DNTN')
labels <- c('T-alloch', 'A-autoch', 'HIX-alloch', 'BIX-autoch',
            'DOC',
            'NH4', 'NO3', 'SRP',
            'TN', 'TP', 'Chl-a',
            'TNTP', 'DPTP', 'DNTN')
names(labels) <- levels
time_long$variable <- factor(time_long$variable, levels = levels)


CV_site <- data[data$distance_from_stream>0,] %>% group_by(Reservoir, Date) %>% 
  mutate(CV_chl = cv(Chla_ugL, na.rm = TRUE)) %>% 
  mutate(CV_SRP = cv(SRP_ugL, na.rm = TRUE)) %>% 
  mutate(CV_NO3 = cv(NO3NO2_ugL, na.rm = TRUE)) %>% 
  mutate(CV_NH4 = cv(NH4_ugL, na.rm = TRUE)) %>% 
  mutate(CV_TP = cv(TP_ugL, na.rm = TRUE)) %>% 
  mutate(CV_TN = cv(TN_ugL, na.rm = TRUE)) %>% 
  mutate(CV_TNTP = cv(TN_TP, na.rm = TRUE)) %>% 
  mutate(CV_A = cv(A, na.rm = TRUE)) %>% 
  mutate(CV_T = cv(T, na.rm = TRUE)) %>% 
  mutate(CV_DOC = cv(DOC_mgL, na.rm = TRUE)) %>% 
  mutate(CV_DPTP = cv(DP_TP, na.rm = TRUE)) %>% 
  mutate(CV_DNTN = cv(DN_TN, na.rm = TRUE)) %>% 
  select(Date, Reservoir, distance_from_stream, distance_m, CV_chl:CV_DNTN) %>% 
  distinct(Date, Reservoir, .keep_all = TRUE)
CV_site$Date <- as.Date(CV_site$Date)

site_long <- CV_site %>% 
  pivot_longer(CV_chl:CV_DNTN, names_to = 'variable', values_to = 'cv')
site_long$axis <- 'space'
names(labels) <- levels
site_long$variable <- factor(site_long$variable, levels = levels)

## plots
#ggplot(time_long, aes(x = as.factor(Site), y = cv, fill = Reservoir)) +
#  geom_bar(position = 'dodge', stat = 'identity') +
#  facet_wrap(~variable, scale = 'free') 
#
#ggplot(site_long, aes(x = as.factor(month(Date)), y = cv, fill = Reservoir)) +
#  geom_bar(position = 'dodge', stat = 'identity') +
#  facet_wrap(~variable, scale = 'free')
#
#
# combine
long_both <- full_join(time_long, site_long)
vars <- unique(long_both$variable)
long_both$ymax <- NA
new <- long_both[,]
for(i in 1:length(vars)){
  temp <- long_both[long_both$variable==vars[i],]
  temp$ymax <- max(temp$cv, na.rm = TRUE) + 0.5*max(temp$cv, na.rm = TRUE)
  new <- rbind(new, temp)
}

new <- new[-(1:nrow(long_both)),]
long_both <- new
long_both$compare <- paste0(long_both$Reservoir, "", long_both$axis)
long_both$compare <- factor(long_both$compare, levels = c("BVRspace", "FCRspace", "BVRtime",  "FCRtime"  ))

vars_keep <- c( 'CV_chl',  'CV_SRP',  'CV_NO3',  
                'CV_NH4',  'CV_TP',   'CV_TN',    
                'CV_A',    'CV_T',    'CV_DOC')

long_both <- long_both[long_both$variable %in% vars_keep,]

levels <- c('CV_NH4', 'CV_NO3', 'CV_SRP',    
            'CV_DOC', 'CV_TP',  'CV_TN',    
            'CV_chl', 'CV_T', 'CV_A')

labels <- c('a) NH4', 'b) NO3', 'c) SRP',
            'd) DOC', 'e) TN', 'f) TP',
            'g) Chl-a', 'h) T-autoch',  'i) A-alloch') 

names(labels) <- levels
long_both$variable <- factor(long_both$variable, levels = levels)

### space time reservoir box plots
# use stat compare means
ggplot(long_both, aes(x = as.factor(axis), y = cv, fill = Reservoir)) +
  geom_boxplot(outlier.shape = NA) +
  geom_blank(aes(y=ymax, x = as.factor(axis)))+
  facet_wrap(~variable, scales = 'free', labeller = labeller(variable = labels)) +
  geom_point(position=position_jitterdodge(),alpha=0.7, size = 2, aes(color = Reservoir)) + 
  stat_compare_means(label = "p.signif", label.y.npc = 0.75, size = 4) + 
  stat_compare_means(aes(group = axis), 
                     label = "p.signif", label.y.npc = 0.95,
                     size = 4,
                     label.x.npc = 0.5) +
  scale_fill_manual(values = r_col) +
  scale_color_manual(values = r_col) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text = element_text(size = 14),
        axis.text = element_text(size = 14)) +  
  xlab('Axis') +
  ylab('Coefficient of Variation (CV)')


# format CV data into table of min, max, mean for CVspace and CV time
cv_long <- long_both %>% 
  select(Reservoir, variable, axis, cv) 

cv_long <- cv_long %>% 
  group_by(Reservoir, variable, axis) %>% 
  mutate(min = min(cv, na.rm = TRUE)) %>% 
  mutate(max = max(cv, na.rm = TRUE)) %>% 
  mutate(mean = mean(cv, na.rm = TRUE)) 

table <- cv_long %>% 
  select(Reservoir, variable, min, max, mean) %>% 
  distinct(Reservoir, variable, .keep_all = TRUE) %>% 
  mutate(variable = gsub("CV_", "", variable)) %>% 
  mutate(min = round(min, digits = 2)) %>% 
  mutate(max = round(max, digits = 2)) %>% 
  mutate(mean = round(mean, digits = 2)) 

table <- table %>% 
  pivot_wider(names_from = axis, values_from = min:mean)

table <- table %>% 
  select(Reservoir, variable, min_time, max_time, mean_time, min_space, max_space, mean_space)

colnames(table) <- c('Reservoir', 'Variable', 'Min Time', 'Max Time', 'Mean Time', 'Min Space', 'Max Space', 'Mean Space')

write.csv(table, './Data/summary_stats_cv.csv', row.names = FALSE)




##################################################################################3
## other exploratory plots
png('./Figures/space_time_all4.png', width = 1100, height = 800)
long_both %>% 
  mutate(gr=interaction(Reservoir, axis, sep = " ")) %>% 
  mutate(gr = factor(gr, levels = c("BVR space", "FCR space", "BVR time",  "FCR time"  ))) %>% 
  {ggplot(data=.,aes(x = gr,  y = cv, fill = Reservoir)) +
      geom_boxplot(size = 1) +
      facet_wrap(~variable, scales = 'free') +
      #geom_blank(aes(y=ymax, x = as.factor(axis)))+
      #stat_summary(fun.y = mean, geom = "bar") +
      # stat_summary(aes(col = Reservoir), fun.data = "mean_se", geom = "errorbar", width=0.6)+
      geom_point(position=position_jitterdodge(),alpha=0.3, size = 3) + 
      ggsignif::geom_signif(comparisons = list(c('FCR space', 'BVR space'),
                                               c('FCR time', 'BVR time'),
                                               c('BVR time', 'BVR space'), 
                                               c('FCR time', 'FCR space')
      ),
      size = 1, textsize = 8,
      step_increase = 0.09,
      test = "wilcox.test", 
      #test.args = list(exact = FALSE),
      map_signif_level = TRUE) +
      scale_fill_manual(values = r_col) +
      scale_x_discrete(labels = c('Space', '', 'Time', '')) +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.text.x = element_text(hjust = 0, size = 12),
            axis.ticks.x = element_blank(),
            axis.text = element_text(size = 12)
            )+
      xlab('Axis') +
      ylab('Coefficient of Variation (CV)')}
dev.off()

## and a version where the file doesn't get saved
long_both %>% 
  mutate(gr=interaction(Reservoir, axis, sep = " ")) %>% 
  mutate(gr = factor(gr, levels = c("BVR space", "FCR space", "BVR time",  "FCR time"  ))) %>% 
  {ggplot(data=.,aes(x = gr,  y = cv, fill = Reservoir)) +
      geom_boxplot() +
      facet_wrap(~variable, scales = 'free') +
      #geom_blank(aes(y=ymax, x = as.factor(axis)))+
      #stat_summary(fun.y = mean, geom = "bar") +
      # stat_summary(aes(col = Reservoir), fun.data = "mean_se", geom = "errorbar", width=0.6)+
      geom_point(position=position_jitterdodge(),alpha=0.3) + 
      ggsignif::geom_signif(comparisons = list(c('FCR space', 'BVR space'),
                                               c('FCR time', 'BVR time'),
                                               c('BVR time', 'BVR space'), 
                                               c('FCR time', 'FCR space')
      ),
      #size = 1, textsize = 8,
      step_increase = 0.09,
      test = "wilcox.test", 
      #test.args = list(exact = FALSE),
      map_signif_level = TRUE) +
      scale_fill_manual(values = r_col) +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            axis.text.x = element_text(hjust = 0),
            axis.ticks.x = element_blank()) +
      scale_x_discrete(labels = c('Space', '', 'Time', '')) +
      xlab('Axis') +
      ylab('Coefficient of Variation (CV)')}


long_both %>% 
  mutate(gr=interaction(Reservoir, axis, sep = " ")) %>% 
  mutate(gr = factor(gr, levels = c("BVR space", "FCR space", "BVR time",  "FCR time"  ))) %>% 
  {ggplot(data=.,aes(x = gr,  y = cv, fill = Reservoir)) +
      geom_boxplot() +
      facet_wrap(~variable, scales = 'free') +
      geom_point(position=position_jitterdodge(),alpha=0.3) + 
      ggsignif::geom_signif(comparisons = list(c('FCR space', 'BVR space'),
                                               c('FCR time', 'BVR time')),
      step_increase = 0.09,
      test = "wilcox.test", 
      map_signif_level = TRUE) +
      scale_fill_manual(values = r_col) +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            axis.text.x = element_text(hjust = 0),
            axis.ticks.x = element_blank()) +
      scale_x_discrete(labels = c('Space', '', 'Time', '')) +
      xlab('Axis') +
      ylab('Coefficient of Variation (CV)')}

###### other stuff...




library(ggpubr)
ggplot(long_both, aes(x = as.factor(axis), y = cv, fill = Reservoir)) +
  geom_boxplot() +
  facet_wrap(~variable, scale = 'free') +
  geom_point(position=position_jitterdodge(),alpha=0.3) + 
  stat_compare_means(comparisons = comparisons, label = "p.format", label.y.npc = 0.8)+
  stat_compare_means(label = "p.format", label.y.npc = 0.8) + 
  stat_compare_means(aes(group = axis), 
                     label = "p.format", label.y.npc = 0.9)

ggplot(long_both, aes(x = as.factor(axis), y = cv)) +
  geom_boxplot() +
  facet_wrap(~variable, scale = 'free') +
  geom_jitter(width=0.1,alpha=0.2) + 
  stat_compare_means(label = "p.format", label.y.npc = 0.8) 



ggplot(long_both, aes(x = compare, y = cv, fill = Reservoir)) +
  geom_boxplot() +
  facet_wrap(~variable, scales= 'free') +
  ggsignif::geom_signif(comparisons = list(c('FCR space', 'BVR space'),
                                 c('FCR time', 'BVR time'),
                                 c('BVR time', 'BVR space'), 
                                 c('FCR time', 'FCR space')),
              step_increase = 0.09,
              test = "wilcox.test", 
              test.args = list(exact = FALSE),
              map_signif_level = TRUE,
              y_position = long_both$ymax)


##########################################################################################################################
### boxplots of concentrations
ggplot(data = long, aes(x = variable, y = value, fill = as.factor(Reservoir))) +
  facet_wrap(~variable, scales = 'free') +
  geom_boxplot() +
  geom_point(position=position_jitterdodge(),alpha=0.15) +
  stat_compare_means(method = 'wilcox.test', label = "p.signif")
  

ggplot(data = long, aes(x = as.factor(distance_from_stream), y = value, fill = as.factor(distance_from_stream))) +
  facet_wrap(~variable, scales = 'free') +
  geom_boxplot() +
  geom_point(position=position_jitterdodge(),alpha=0.2) +
  theme(legend.position = 'none')+
  stat_compare_means(method = 'wilcox.test', label = "p.signif")

ggplot(data = long[long$distance_from_stream>0,], aes(x = as.factor(distance_from_stream), y = value, fill = as.factor(distance_from_stream))) +
  facet_wrap(~variable, scales = 'free') +
  geom_boxplot() +
  geom_point(position=position_jitterdodge(),alpha=0.3) +
  theme(legend.position = 'none')

ggplot(data = long[long$distance_from_stream>0,], aes(x = as.factor(month(Date)), y = value, fill = as.factor(Date))) +
  facet_wrap(~variable, scales = 'free') +
  geom_boxplot() +
  geom_point(position=position_jitterdodge(),alpha=0.3) +
  theme(legend.position = 'none')

ggplot(data = long, aes(x = as.factor(month(Date)), y = value, fill = as.factor(Date))) +
  facet_wrap(~variable, scales = 'free') +
  geom_boxplot() +
  geom_point(position=position_jitterdodge(),alpha=0.2) +
  theme(legend.position = 'none')

ggplot(data = long[long$distance_from_stream>0,], aes(x = as.factor(distance_from_stream), 
                                                      y = value, 
                                                      fill = as.factor(Reservoir))) +
  facet_wrap(~variable, scales = 'free') +
  geom_boxplot() +
  geom_point(position=position_jitterdodge(),alpha=0.3) #+
  theme(legend.position = 'none')
  #geom_jitter(width=0.1,alpha=0.2)  


ggplot(data = long[long$distance_from_stream>0,], aes(x = as.factor(month(Date)), y = value, fill = as.factor(Reservoir))) +
  facet_wrap(~variable, scales = 'free') +
  geom_boxplot() +
  geom_jitter(width=0.1,alpha=0.2) +
  theme(legend.position = 'none')

##### and WITH the stream data
ggplot(data = long, aes(x = as.factor(distance_from_stream), 
                                                      y = value, 
                                                      fill = as.factor(Reservoir))) +
  facet_wrap(~variable, scales = 'free') +
  geom_boxplot() +
  geom_point(position=position_jitterdodge(),alpha=0.3) #+
theme(legend.position = 'none')
#geom_jitter(width=0.1,alpha=0.2)  


ggplot(data = long, aes(x = as.factor(month(Date)), y = value, fill = as.factor(Reservoir))) +
  facet_wrap(~variable, scales = 'free') +
  geom_boxplot() +
  geom_point(position=position_jitterdodge(),alpha=0.2) #+
  theme(legend.position = 'none')




# use ggsignif
ggplot(long_both, aes(x = as.factor(axis), y = cv, fill = Reservoir)) +
  geom_boxplot() +
  geom_blank(aes(y=ymax, x = as.factor(axis)))+
  facet_wrap(~variable, scales = 'free') +
  geom_point(position=position_jitterdodge(),alpha=0.3) +
  ggsignif::geom_signif(comparisons = combn(sort(unique(as.character(long_both$compare))),2, simplify = F),
                        step_increase = 0.08,test = "wilcox.test", test.args = list(exact = FALSE))
  
  
####### do again WITH stream data
CV_time <- data %>% group_by(Reservoir, Site) %>% 
  mutate(CV_chl = cv(Chla_ugL, na.rm = TRUE)) %>% 
  mutate(CV_SRP = cv(SRP_ugL, na.rm = TRUE)) %>% 
  mutate(CV_NO3 = cv(NO3NO2_ugL, na.rm = TRUE)) %>% 
  mutate(CV_NH4 = cv(NH4_ugL, na.rm = TRUE)) %>% 
  mutate(CV_TP = cv(TP_ugL, na.rm = TRUE)) %>% 
  mutate(CV_TN = cv(TN_ugL, na.rm = TRUE)) %>% 
  mutate(CV_TNTP = cv(TN_TP, na.rm = TRUE)) %>% 
  mutate(CV_A = cv(A, na.rm = TRUE)) %>% 
  mutate(CV_T = cv(A, na.rm = TRUE)) %>% 
  mutate(CV_HIX = cv(HIX, na.rm = TRUE)) %>% 
  mutate(CV_BIX = cv(BIX, na.rm = TRUE)) %>% 
  mutate(CV_DPTP = cv(DP_TP, na.rm = TRUE)) %>% 
  mutate(CV_DNTN = cv(DN_TN, na.rm = TRUE)) %>% 
  select(Date, Reservoir, distance_from_stream, distance_m, CV_chl:CV_DNTN) %>% 
  distinct(Site, Reservoir, .keep_all = TRUE)
CV_time$Date <- as.Date(CV_time$Date)

time_long <- CV_time %>% 
  pivot_longer(CV_chl:CV_DNTN, names_to = 'variable', values_to = 'cv')
time_long$axis <- 'time'

CV_site <- data %>% group_by(Reservoir, Date) %>% 
  mutate(CV_chl = cv(Chla_ugL, na.rm = TRUE)) %>% 
  mutate(CV_SRP = cv(SRP_ugL, na.rm = TRUE)) %>% 
  mutate(CV_NO3 = cv(NO3NO2_ugL, na.rm = TRUE)) %>% 
  mutate(CV_NH4 = cv(NH4_ugL, na.rm = TRUE)) %>% 
  mutate(CV_TP = cv(TP_ugL, na.rm = TRUE)) %>% 
  mutate(CV_TN = cv(TN_ugL, na.rm = TRUE)) %>% 
  mutate(CV_TNTP = cv(TN_TP, na.rm = TRUE)) %>% 
  mutate(CV_A = cv(A, na.rm = TRUE)) %>% 
  mutate(CV_T = cv(A, na.rm = TRUE)) %>% 
  mutate(CV_HIX = cv(HIX, na.rm = TRUE)) %>% 
  mutate(CV_BIX = cv(BIX, na.rm = TRUE)) %>% 
  mutate(CV_DPTP = cv(DP_TP, na.rm = TRUE)) %>% 
  mutate(CV_DNTN = cv(DN_TN, na.rm = TRUE)) %>% 
  select(Date, Reservoir, distance_from_stream, distance_m, CV_chl:CV_DNTN) %>% 
  distinct(Date, Reservoir, .keep_all = TRUE)
CV_site$Date <- as.Date(CV_site$Date)

site_long <- CV_site %>% 
  pivot_longer(CV_chl:CV_DNTN, names_to = 'variable', values_to = 'cv')
site_long$axis <- 'space'

# combine
long_both <- full_join(time_long, site_long)

ggplot(long_both, aes(x = as.factor(axis), y = cv, fill = Reservoir)) +
  geom_boxplot() +
  facet_wrap(~variable, scale = 'free') +
  geom_point(position=position_jitterdodge(),alpha=0.3) +
  ggtitle('including stream data')

ggboxplot(long_both, x = 'axis',
          y = 'cv', fill = 'Reservoir',
          facet.by = 'variable',
          scales = 'free') +
  stat_compare_means(label = "p.format",
                     label.y = max(long_both$cv) - 0.5*max(long_both$cv))
