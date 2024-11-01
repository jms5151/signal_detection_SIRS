# Load required libraries
library(GGally)
library(ggplot2)

# load data
reorganized_data <- readRDS('../data/sim_summaries/power_summary.RData')

# format data to look for combination of metrics
comb_metrics <- reorganized_data[,c('suscept', 'regime', 'intensity', 'duration', 'metric', 'mean_diff')] %>%
  spread(key = metric, value = mean_diff)

# subset to regime
dry_s_min <- subset(comb_metrics, regime == 'wet' & suscept == 'S_max')  

# pair plots
ggpairs(dry_s_min[,5:9]) + theme_bw() + ggtitle('Wet, Smax')

