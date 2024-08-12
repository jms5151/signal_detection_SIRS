# load packages
library(tidyverse)

# load extreme event data from scoping review
csid <- read.csv('../CSID_scoping_review_SI_edited.csv')
csid$City <- paste(csid$Study.Area, csid$Country, sep = ', ')

# load Koppen names for each event
koppen <- read.csv('../EE_Koppen_filled.csv')

# join data
csid2 <- csid %>% left_join(koppen)

# format dates
csid2[,c('Date_start', 'Date_end', 'Date_peak')] <- lapply(csid2[,c('Date_start', 'Date_end', 'Date_peak')], function(x) as.Date(x, '%m/%d/%Y'))

# list historical climate data files
clim_files_path <- '../data/CSID_ex_events_climate/'
clim_files <- list.files(clim_files_path)

# Add percentiles from historical data
for(i in 1:nrow(csid2)){
  if(is.na(csid2$Percentile[i])){
    ee_id <- csid2$ID[i]
    fileID <- which(grepl(paste0('ID', ee_id, '_'), clim_files))
    clim_data <- read.csv(paste0(clim_files_path, clim_files[fileID]))
    clim_data$Date <- as.Date(clim_data$time)
    if(!is.na(csid2$Date_peak[i])){
      ee_value <- clim_data$X0[which(clim_data$Date == csid2$Date_peak[i])]
    } else {
      ee_value <- max(clim_data$X0[which(clim_data$Date == csid2$Date_start[i]):which(clim_data$Date == csid2$Date_end[i])])
    }
    clim_distr <- ecdf(clim_data$X0)
    csid2$Percentile[i] <- round(clim_distr(ee_value) * 100)
  }
}

# save
csid2$Extreme_event <- csid2$Extreme.climate.event
x <- csid2[, c('ID', 'City', 'lon', 'lat', 'Extreme_event', 'Climate_variable', 'Percentile', 'Koppen_Name')]
write.csv(x, file = '../data/extreme_events.csv', row.names = F)

# plot
boxplot(csid2$Percentile~csid2$Koppen_Name)
boxplot(csid2$Percentile~csid2$Disease)
boxplot(csid2$Percentile~csid2$Extreme.climate.event)
