library(tidyverse)
library(ggplot2)
library(RColorBrewer)

df <- readRDS('../data/ee_summary_all.RData')


# list of plots I want to make:
# 1. heatmaps across countries/climate regimes (vbd = 6, wbd = 5)
# 1a. Plot consisting of 1 metric, 1 ee timing, and 1 time window for comparison (12 metrics x 3 timings x 2 windows x 2 disease systems = 144 plots eek)

disease_system <- unique(df$diseaseSystem)
time_window <- unique(df$timeWindow)
metrics <- unique(df$Variable)
ee_timing <- unique(df$ee_timing)

savedir <- '../figures/heatmaps/'

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

ggplot(test, aes(x = Variable, y = region, fill = powerAbove80)) +
  geom_tile() +
  scale_fill_gradientn(colours = myPalette(100), limits = c(0, 100)) +
  facet_wrap(~ee_timing+timeWindow)


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
site_data <- list.files('../data/', full.names = T)
site_data <- site_data[grepl('precip|temp', site_data)]

library(tidyverse)
distr <- data.frame()

for(i in site_data){
  x <- readRDS(i)
  x0 <- x$prob_ee_hist
  x0$time_period <- 'A. historical'
  x1 <- x$prob_ee_fut
  x1$time_period <- 'B. future'
  x2 <- rbind(x0, x1)
  # colnames(x0)[4] <- 'hist_prob'
  # colnames(x1)[4] <- 'fut_prob'
  # x2 <- x0[,c('Duration', 'Intensity', 'hist_prob')] %>%
    # full_join(x1[,c('Duration', 'Intensity', 'fut_prob')])
  # x2$prob_diff <- x2$fut_prob - x2$hist_prob
  x2$country <- gsub('../data/|metrics_|precip_|temp_|.RData$', '', i)
  x2$ModelType <- ifelse(grepl('precip', i)==T, 'wbd', 'vbd')
  distr <- rbind(distr, x2)
}

rain = subset(distr, ModelType == 'wbd' & time_period == 'A. historical')
temp = subset(distr, ModelType == 'vbd' & time_period == 'A. historical')

# savedir2 <- '../figures/heatmaps/'

# Calculate breaks and labels for the legend
log_breaks <- log(seq(1, 1000, length.out = 3)) # Adjust based on your data range and desired breaks
labels <- round(exp(log_breaks)) # Convert back to original scale for labels

ggplot(temp, aes(x = Intensity, y = Duration, fill = log(ee_count))) + 
  geom_tile() + # Use geom_tile for heatmap
  scale_fill_gradientn(colours = myPalette(100), 
                       breaks = log_breaks, 
                       labels = labels) +
  theme_bw() +
  facet_wrap(~country) +
  # facet_grid(country ~ time_period) +
  labs(fill = "Event Count\n(log space)") #+
  xlim(0,10) +
  ylim(0,20)


## Difference
ggplot(temp, aes(x = Intensity, y = Duration, fill = prob_diff)) + 
  geom_tile() + # Use geom_tile for heatmap
  scale_fill_gradientn(colours = myPalette(100)) +
  theme_bw() +
  facet_wrap(~country) #+
  xlim(0,100)
