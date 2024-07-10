library(tidyverse)
library(ggplot2)
library(RColorBrewer)

df <- readRDS('../data/sim_summaries/power_summary.RData')

rain <- subset(df, regime == 'dry' | regime == 'moderate' | regime == 'wet')
rain_add <- subset(rain, type == 'additive')
rain_multi <- subset(rain, type == 'multiplicative')
temp <- subset(df, regime == 'temperate' | regime == 'warm' | regime == 'hot')

makeheatmaps <- function(dfx, climvar){
  myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
  sc = scale_fill_gradientn(colours = myPalette(100), limits = c(0, 1))
  metrics <- unique(dfx$metric)
  for(i in metrics){
    x <- subset(dfx, metric == i)
    p <- ggplot(x, aes(x = intensity, y = duration, fill = power)) +
      geom_tile() +
      sc +
      theme_bw() +
      facet_grid(suscept~regime, scales = 'free') +
      ggtitle(i)
    savedir <- '../figures/heatmaps_regimes/'
    filePath <- paste0(savedir, climvar, '_', i, '.pdf')
    ggsave(p, file = filePath, width = 10, height = 6.5)
    }
}

makeheatmaps(dfx = rain_add, climvar = 'rain')
makeheatmaps(dfx = rain_multi, climvar = 'rain_multi')
makeheatmaps(dfx = temp, climvar = 'temp')

# create heatmaps of most robust metrics
dfpow <- readRDS('../data/sim_summaries/highest_power_summary.RData')

prain <- subset(dfpow, regime == 'dry' | regime == 'moderate' | regime == 'wet')
prain_add <- subset(prain, type == 'additive')
prain_multi <- subset(prain, type == 'multiplicative')
ptemp <- subset(dfpow, regime == 'temperate' | regime == 'warm' | regime == 'hot')

makerobustheatmap <- function(dfx, climvar){
  p <- ggplot(dfx, aes(x = intensity, y = duration, fill = metric)) +
    geom_tile() +
    theme_bw() +
    facet_grid(suscept~regime, scales = 'free')
  savedir <- '../figures/heatmaps_regimes/'
  filePath <- paste0(savedir, climvar, '_robust', '.pdf')
  ggsave(p, file = filePath, width = 10, height = 6.5) 
}

makerobustheatmap(dfx = prain_add, climvar = 'rain')
makerobustheatmap(dfx = prain_multi, climvar = 'rain_multi')
makerobustheatmap(dfx = temp, climvar = 'temp')
