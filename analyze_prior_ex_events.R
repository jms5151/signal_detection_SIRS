# load packages
library(tidyverse)

# load extreme event data from scoping review
csid <- read.csv('../CSID_scoping_review_SI_edited.csv')
csid$City <- paste(csid$Study.Area, csid$Country, sep = ', ')

# load agreement/evidence from scoping review
ae <- read.csv('../CSID_Agr_Evid.csv')

# load Koppen names for each event
koppen <- read.csv('../EE_Koppen_filled.csv')

# join data
csid2 <- csid %>%
  left_join(ae) %>%
  left_join(koppen) %>%
  mutate(Climate_threshold_80 = NA, Climate_value = NA) %>%
  filter(!is.na(ID))

# format dates
csid2[,c('Date_start', 'Date_end', 'Date_peak')] <- lapply(csid2[,c('Date_start', 'Date_end', 'Date_peak')], function(x) as.Date(x, '%m/%d/%Y'))

# list historical climate data files
clim_files_path <- '../data/CSID_ex_events_climate/'
clim_files <- list.files(clim_files_path)

# Add percentiles from historical data
for(i in 1:nrow(csid2)){
  ee_id <- csid2$ID[i]
  fileID <- which(grepl(paste0('ID', ee_id, '_'), clim_files))
  clim_data <- read.csv(paste0(clim_files_path, clim_files[fileID]))
  clim_data$Date <- as.Date(clim_data$time)
  if(csid2$Climate_variable[i] == 'Rainfall'){
    clim_data$YM <- format(clim_data$Date, '%Y-%m')
    clim_summary <- clim_data %>%
      group_by(YM) %>%
      summarise(X0sum = sum(X0, na.rm = T)) %>%
      as.data.frame()
  csid2$Climate_threshold_80[i] <- quantile(clim_summary$X0sum, 0.8)
  if(!is.na(csid2$Date_peak[i])){
    csid2$Climate_value[i] <- clim_summary$X0sum[which(clim_summary$YM == format(csid2$Date_peak[i], '%Y-%m'))]
    } else if(!is.na(csid2$Date_start[i]) & !is.na(csid2$Date_end[i])){
      csid2$Climate_value[i] <- max(clim_summary$X0sum[which(clim_summary$YM == format(csid2$Date_start[i], '%Y-%m')):which(clim_summary$YM == format(csid2$Date_end[i], '%Y-%m'))])
    } else {
      csid2$Climate_value[i] <- NA
    }  
  if(is.na(csid2$Percentile[i])){
    clim_distr <- ecdf(clim_summary$X0sum)
    csid2$Percentile[i] <- round(clim_distr(csid2$Climate_value[i]) * 100)
    }
  }
  else { # if temperature data
    if(!is.na(csid2$Date_peak[i])){
      csid2$Climate_value[i] <- clim_data$X0[which(clim_data$Date == csid2$Date_peak[i])]
    } else if(!is.na(csid2$Date_start[i]) & !is.na(csid2$Date_end[i])){
      csid2$Climate_value[i] <- max(clim_data$X0[which(clim_data$Date == csid2$Date_start[i]):which(clim_data$Date == csid2$Date_end[i])])
    } else {
      csid2$Climate_value[i] <- NA
    }
    csid2$Climate_threshold_80[i] <- quantile(clim_data$X0, 0.8)
    if(is.na(csid2$Percentile[i])){
      clim_distr <- ecdf(clim_data$X0)
      csid2$Percentile[i] <- round(clim_distr(csid2$Climate_value[i]) * 100)
    }
  }
}
  
# average percentiles across studies
# x <- csid2 %>%
#   group_by(ID, City, lon, lat, Disease, Climate_variable, Outbreak_risk, Extreme_climate_event, Koppen_Name, Agreement, Evidence) %>%
#   summarise(Percentile = median(Percentile)
#             , Clim = median(Climate_median)
#             ) %>%
#   as.data.frame()
# 
# # round and average by clim and then plot?
# xx <- subset(x, Climate_variable == 'Rainfall' & Disease == 'Cholera')
# plot(xx$Clim, xx$Percentile, pch = 16, col = as.factor(xx$Outbreak_risk))
# 
# xx <- subset(csid2, Climate_variable == 'Temperature')
# plot(xx$Climate_median, xx$Percentile, pch = 16, col = as.factor(xx$Disease))
# 
# x1 <- x %>%
#   mutate(Climate2 = round(Clim, 1)) %>%
#   filter(Climate_variable == 'Rainfall') %>%
#   group_by(Climate2) %>%
#   mutate(mid = quantile(Percentile, 0.5)) %>% as.data.frame()
#             , lwr = quantile(Percentile, 0.25)
#             , upr = quantile(Percentile), 0.75)
# 
# plot(x1$Climate2, x1$mid, col = as.factor(xx$Outbreak_risk))
x <- csid2

# format text for plotting
x$Outbreak_risk <- gsub('No.*', 'No', x$Outbreak_risk)
x$Outbreak_risk <- gsub('Yes/Mixed', 'Mixed', x$Outbreak_risk)
x$Outbreak_risk <- gsub('Yes.*', 'Yes', x$Outbreak_risk)

x$Koppen_Name[x$City == 'Nationwide, Singapore'] <- 'Af'
x$Koppen_Name[x$City == 'Nationwide, Brazil'] <- 'Aw' # based on Mato Grosso do Sul where dengue is high

x$Disease <- gsub('Malaria.*', 'Malaria', x$Disease)

x$Koppen_Group <- gsub('A.*', 'Tropical', x$Koppen_Name)
x$Koppen_Group <- gsub('B.*', 'Dry', x$Koppen_Group)
x$Koppen_Group <- gsub('C.*', 'Temperate', x$Koppen_Group)
x$Koppen_Group <- gsub('D.*', 'Continental', x$Koppen_Group)

x$Koppen_Subgroup <- gsub('Af', 'Rainforest', x$Koppen_Name)
x$Koppen_Subgroup <- gsub('Am', 'Monsoon', x$Koppen_Subgroup)
x$Koppen_Subgroup <- gsub('As', 'Savanna,\n dry summer', x$Koppen_Subgroup)
x$Koppen_Subgroup <- gsub('Aw', 'Savanna,\n dry winter', x$Koppen_Subgroup)
x$Koppen_Subgroup <- gsub('BSh', 'Semi-Arid\n Steppe, Hot', x$Koppen_Subgroup)
x$Koppen_Subgroup <- gsub('BWh', 'Arid Desert,\n Hot', x$Koppen_Subgroup)
x$Koppen_Subgroup <- gsub('BWh', 'Arid Desert,\n Hot', x$Koppen_Subgroup)
x$Koppen_Subgroup <- gsub('Cfa', 'No dry season,\n Hot summer', x$Koppen_Subgroup)
x$Koppen_Subgroup <- gsub('Cwa', 'Dry winter,\n Hot summer', x$Koppen_Subgroup)
x$Koppen_Subgroup <- gsub('Dfa', 'No dry season,\n Hot summer', x$Koppen_Subgroup)
x$Koppen_Subgroup <- gsub('Dfb', 'No dry season,\n Warm summer', x$Koppen_Subgroup)

x$Extreme_climate_event[x$Extreme_climate_event == 'Cyclone/Hurricane/Typhoon'] <- 'Cyclone/\nHurricane/Typhoon'

x$Agreement[x$Agreement == ""] <- NA

# x$Climate_threshold_80[x$Climate_threshold_80 == 1] <- 0.330750

# save
write.csv(x, file = '../data/extreme_events.csv', row.names = F)
