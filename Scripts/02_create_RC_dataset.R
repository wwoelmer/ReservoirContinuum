# combine all data together into one dataset and define categorical variables

# for now, reading in the dataset I've already created and adding in EEMs
# but NEED to script in the creation of all of these pieces of data from the published EDI files

data <-read.csv('./Data/unscripted_files/continuum_ww.csv')
data$Date <- as.Date(data$DateTime)
data$connectivity <- as.factor(data$connectivity)

# define specifics for subsetting to RC days
res <- c('FCR', 'BVR')
sites <- c('01', '20', '30', '45', '50', '100', '200', '99', '101', '102')
dates <- c(as.Date("2019-04-29"), 
           as.Date("2019-05-30"),
           as.Date("2019-06-27"),
           as.Date("2019-07-18"),
           as.Date("2019-08-22"),
           as.Date("2019-09-20"),
           as.Date("2019-10-04"))

eems <- read.csv('./Data/eems_edi.csv')
eems$DateTime <- as.Date(eems$DateTime)
eems$res_site <- paste0(eems$Reservoir, '_', eems$Site)
eems <- eems[eems$Site %in% sites,]
eems <- eems[eems$Reservoir %in% res,]
eems <- eems[eems$DateTime %in% dates,]
eems <- eems %>% 
  select(Reservoir:DateTime, A, T, HIX, BIX) %>%
  group_by(Reservoir, Site, DateTime) 
eems <- na.omit(eems)
colnames(eems)[3] <- 'Date'

data <- left_join(data, eems)
# why some NAs in EEMs data?

write.csv(data, './Data/continuum_ww.csv', row.names = FALSE)
