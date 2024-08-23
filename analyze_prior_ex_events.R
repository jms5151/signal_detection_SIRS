# load packages
library(tidyverse)
library(ggplot2)
library(patchwork)

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
  left_join(koppen)

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

# average percentiles across studies
x <- csid2 %>%
  group_by(ID, City, lon, lat, Disease, Climate_variable, Outbreak_risk, Extreme_climate_event, Koppen_Name, Agreement, Evidence) %>%
  summarise(Percentile = median(Percentile))

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

# save
write.csv(x, file = '../data/extreme_events.csv', row.names = F)

x <- read.csv('../data/extreme_events.csv')
x$Percentile[x$Extreme_climate_event == 'Drought'] <- 100 - x$Percentile[x$Extreme_climate_event == 'Drought']

# Plot
plotFun <- function(xName, colorName, custom_colors, colorLegendName, titleName){
  p <- ggplot(x, aes(x = xName, y = Percentile, color = colorName)) +
    geom_hline(yintercept = c(10, 90), linetype = 'dashed', color = 'grey') +
    geom_boxplot() +
    geom_jitter(width = 0.2, size = 3) +
    scale_color_manual(values = custom_colors) +
    theme_bw() +
    labs(title = titleName,
         x = '',
         y = 'Extreme event (percentile)',
         color = colorLegendName)
  return(p)
}

# custom_colors_2 <- c('High' = 'red', 'Medium' = 'yellow', 'Low' = 'black')
# 
# plotFun(xName = x$Extreme_climate_event, colorName = x$Agreement, custom_colors = custom_colors_2, colorLegendName = 'Agreement', titleName = '')
# plotFun(xName = x$Extreme_climate_event, colorName = x$Evidence, custom_colors = custom_colors_2, colorLegendName = 'Evidence', titleName = '')
# plotFun(xName = x$Disease, colorName = x$Evidence, custom_colors = custom_colors_2, colorLegendName = 'Evidence', titleName = '')

# x$Percentile <- x$Percentile/100
custom_colors_1 <- c('Mixed' = 'darkgreen', 'No' = 'black', 'Yes' = 'orange')

# can add shape to show disease or climate event, but gets really complicated to look at
koppen_plot <- plotFun(xName = x$Koppen_Subgroup, colorName = x$Outbreak_risk, custom_colors = custom_colors_1, colorLegendName = 'Outbreak risk', titleName = 'Extreme event triggered outbreaks by Koppen climate regime') +
  facet_grid( ~ Koppen_Group, scales = 'free_x', space = 'free')

disease_plot <- plotFun(xName = x$Disease, colorName = x$Outbreak_risk, custom_colors = custom_colors_1, colorLegendName = 'Outbreak risk', titleName = 'Extreme event triggered outbreaks by disease type') 

climate_plot <- plotFun(xName = x$Extreme_climate_event, colorName = x$Outbreak_risk, custom_colors = custom_colors_1, colorLegendName = 'Outbreak risk', titleName = 'Extreme event triggered outbreaks by climate event') 

# combine plots
combined_plot <- koppen_plot / (disease_plot | climate_plot) + 
  plot_layout(guides = 'collect')

# Display the combined plot
combined_plot

# save
ggsave(filename = '../figures/csid_comparison.pdf', plot = combined_plot, width = 13, height = 6.5)
