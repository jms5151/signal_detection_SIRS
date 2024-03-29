library(tidyverse)
library(ggplot2)
library(RColorBrewer)

df <- readRDS('../data/ee_summary_all.RData')
# df$magnitude

# list of plots I want to make:
# 1. heatmaps across countries/climate regimes (vbd = 6, wbd = 5)
# 1a. Plot consisting of 1 metric, 1 ee timing, and 1 time window for comparison (12 metrics x 3 timings x 2 windows x 2 disease systems = 144 plots eek)

disease_system <- unique(df$diseaseSystem)
time_window <- unique(df$timeWindow)
metrics <- unique(df$Variable)
ee_timing <- unique(df$ee_timing)

savedir <- '../figures/heatmaps2/'

myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))

for(i in disease_system){
  for(j in time_window){
    for(k in metrics){
      for(l in ee_timing){
        x <- subset(df, diseaseSystem == i & timeWindow == j & Variable == k & ee_timing == l)
        filePath <- paste0(savedir, paste(i, j, k, l, sep = '_'), '.pdf') 
        if(k == 'Return_time' | k == 'CE_correlation'){
          sc = scale_fill_gradientn(colours = myPalette(100), limits = c(0, 1))
        } else {
          sc = scale_fill_gradientn(colours = myPalette(100))
        }
        p <- ggplot(x, aes(x = magnitude, y = duration, fill = Value)) +
          geom_tile() +
          sc +
          theme_classic() +
          facet_wrap(~region) +
          ggtitle(paste(i, j, k, l, sep = ', '))
        ggsave(p, file = filePath, width = 10, height = 6.5)
      }
    }
  }
}



powersummary <- df %>%
  group_by(diseaseSystem, region, ee_timing, timeWindow, Variable) %>%
  summarise(powerAbove80 = round(sum(Value > 0.80)/length(Value) * 100)) %>%
  filter(!(Variable %in% c('Return_time', 'CE_correlation', 'min_Re')))
# should possibly be weighted by event count
test <- subset(powersummary, timeWindow == 'short')
test$xcombo <- paste(test$region, test$ee_timing, sep = '-')
ggplot(test, aes(x = Variable, y = xcombo, fill = powerAbove80)) +
  geom_tile(color = 'black') +
  scale_fill_gradientn(colours = myPalette(100), limits = c(0, 100)) +
  # facet_wrap(~ee_timing) +
  xlab('') +
  ylab('') +
  theme_classic()


powersummary$region_ee_timing <- factor(paste(powersummary$region, powersummary$ee_timing, sep = "_"))

test <- subset(powersummary, diseaseSystem == 'wbd' & timeWindow == 'short')

# Step 2: Plotting
ggplot(test, aes(x = Variable, y = region_ee_timing, fill = powerAbove80)) +
  geom_tile() + # Use geom_tile for heatmap
  scale_fill_gradientn(colours = myPalette(100), limits = c(0, 100)) +
  labs(y = "Region and EE Timing", x = "Variable", fill = "Power Above 80") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x axis labels if needed




### distributions of ee events
myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))

site_data <- list.files('../data/', full.names = T)
site_data <- site_data[grepl('climate_metrics', site_data)]

wbdcountries <- c('Sudan', 'Ethiopia', 'India', 'China', 'Haiti')
vbdcountries <- c('Italy', 'Pakistan', 'Philippines', 'Fiji', 'Brazil', 'BurkinaFaso')

library(tidyverse)
distr <- data.frame()

for(i in site_data){
  x <- readRDS(i)
  x0 <- x$ee_hist
  x0$time_period <- 'A. historical'
  x1 <- x$ee_fut
  x1$time_period <- 'B. future'
  x2 <- rbind(x0, x1)
  # colnames(x0)[4] <- 'hist_prob'
  # colnames(x1)[4] <- 'fut_prob'
  # x2 <- x0[,c('Duration', 'Intensity', 'hist_prob')] %>%
    # full_join(x1[,c('Duration', 'Intensity', 'fut_prob')])
  # x2$prob_diff <- x2$fut_prob - x2$hist_prob
  x2$country <- gsub('../data/|climate_metrics_|.RData$', '', i)
  # x2$ModelType <- ifelse(grepl('precip', i)==T, 'wbd', 'vbd')
  x2$ModelType <- ifelse(unique(x2$country) %in% wbdcountries, 'wbd', 'vbd')
  distr <- rbind(distr, x2)
}

rain = subset(distr, ModelType == 'wbd') # & time_period == 'A. historical'
temp = subset(distr, ModelType == 'vbd')

# savedir2 <- '../figures/heatmaps/'



myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))

# Calculate breaks and labels for the legend
log_breaks <- log(seq(1, 1000, length.out = 3)) # Adjust based on your data range and desired breaks
labels <- round(exp(log_breaks)) # Convert back to original scale for labels

ggplot(rain, aes(x = Intensity, y = Duration, fill = log(ee_count))) + 
  geom_tile() + # Use geom_tile for heatmap
  scale_fill_gradientn(colours = myPalette(100), 
                       breaks = log_breaks, 
                       labels = labels) +
  theme_bw() +
  facet_wrap(~country) +
  facet_grid(country ~ time_period) +
  labs(fill = "Event Count\n(log space)") #+
  # xlim(0,7) +
  # ylim(0,40)
  # xlim(0,100) +
  # ylim(0,10)

ggsave('../figures/ee_events/count_comparison_rain.pdf', width = 8, height = 12)

## Difference
ggplot(temp, aes(x = Intensity, y = Duration, fill = prob_diff)) + 
  geom_tile() + # Use geom_tile for heatmap
  scale_fill_gradientn(colours = myPalette(100)) +
  theme_bw() +
  facet_wrap(~country) #+
  xlim(0,100)
  
  
  #### ----- exceedence heatmaps
  source('functions_to_simulate_climate.R')
  
  exceedence <- function(minmean, maxmean, minamp, maxamp, stepsize, clim = 'notrain'){
    clim_mean = seq(minmean, maxmean, stepsize)
    clim_amp = seq(minamp, maxamp, stepsize)
    clim_grid = data.frame(expand.grid('climate_mean' = clim_mean, 'climate_amp' = clim_amp))
    
    for(i in 1:nrow(clim_grid)){
      s <- simulate_seasonal_climate(
        xmin = (clim_grid$climate_mean[i] - clim_grid$climate_amp[i])
        , xmax = (clim_grid$climate_mean[i] + clim_grid$climate_amp[i])
        , xvar = 0
        , seasons = 1
        , years = 1 )
      if(clim == 'rain'){
        s[s<0] <- 0
      }
      clim_grid$p90[i] <- quantile(s[s>0], 0.90)
    }
    return(clim_grid)  
  }
  
  
  T_grid <- exceedence(10, 35, 0, 15, 0.5)
  
  ggplot(T_grid, aes(x = climate_mean, y = climate_amp, fill = p90)) +
    geom_tile() + # color = 'black'
    scale_fill_gradientn(colours = myPalette(100), limits = c(10, 50)) +
    xlab('Mean Temperature') +
    ylab('Seasonal Variation (amplitude)') +
    theme_classic()
  
  # need this think about what the mean and amplitude represent here
  R_grid <- exceedence(1, 100, 0, 50, 1, clim = 'rain')
  
  ggplot(R_grid, aes(x = climate_mean, y = climate_amp, fill = p90)) +
    geom_tile() + # color = 'black'
    scale_fill_gradientn(colours = myPalette(200), limits = c(0, 200)) +
    xlab('Mean Rainfall') +
    ylab('Seasonal Variation (amplitude)') +
    theme_classic()


# values from 'far_and_rr_calculations.R'
rain_values <- c(10, 35, 75)
temp_values <- c(17, 23, 29)    
rgridsub <- subset(R_grid, round(p90) == 35)  
