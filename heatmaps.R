# load libraries
library(tidyverse)
library(ggplot2)
library(RColorBrewer)

# read in data
dfpow <- readRDS('../data/sim_summaries/highest_power_summary.RData')

# format
prain <- subset(dfpow, regime == 'dry' | regime == 'moderate' | regime == 'wet')
prain_add <- subset(prain, type == 'additive')
prain_multi <- subset(prain, type == 'multiplicative')
ptemp <- subset(dfpow, regime == 'temperate' | regime == 'warm' | regime == 'hot')

# format data
format_ee_data <- function(dfx, climvar){
  x <- dfx %>%
    mutate(alpha_value = ifelse(power > 0.8, 1, 0.25),
           susceptibility = ifelse(suscept == 'S_max', 'High pop. susceptibility', 'Low pop. susceptibility')
    )
  x$regime <- paste0(toupper(substring(x$regime, 1, 1)), substring(x$regime, 2))
  if(climvar == 'temp'){
    x$regime <- factor(x$regime, levels = c('Temperate', 'Warm', 'Hot'))
  } else {
    x$regime <- factor(x$regime, levels = c('Dry', 'Moderate', 'Wet'))
  }
  x$metric <- paste0(toupper(substring(x$metric, 1, 1)), substring(x$metric, 2))
  x$metric[x$metric == 'Peakiness'] <- 'Outbreak peakiness (kurtosis)'
  x$metric[x$metric == 'Max_S'] <- 'Serology (pop. susceptibility)'
  x$metric[x$metric == 'peak_timing_cases'] <- 'Timing of outbreak peak'
  x$metric <- gsub('_', ' ', x$metric)
  return(x)  
}
makerobustheatmap <- function(dfx, climvar){
  x <- format_ee_data(dfx, climvar)
  p <- ggplot(x, aes(x = intensity, y = duration, fill = metric, alpha = alpha_value)) +
    geom_tile() +  # Removes borders around the tiles
    scale_alpha_identity() + 
    theme_bw() +
    facet_grid(susceptibility ~ regime, scales = 'free') +
    xlab('Intensity') +
    ylab('Duration') +
    theme(legend.position = 'bottom',
          panel.grid.major = element_blank(),   # Removes major gridlines
          panel.grid.minor = element_blank())
  
  savedir <- '../figures/heatmaps_regimes/'
  filePath <- paste0(savedir, climvar, '_robust', '.png')
  ggsave(p, file = filePath, dpi = 300, width = 10, height = 6.5) 
}

# create and save heat maps
makerobustheatmap(dfx = prain_add, climvar = 'rain')
makerobustheatmap(dfx = prain_multi, climvar = 'rain_multi')
makerobustheatmap(dfx = temp, climvar = 'temp')
