library(dplyr)

# site data
site_data <- list.files('../data/', full.names = T)
site_data <- site_data[grepl('precip|temp', site_data)]

# disease r0 info
cholera.meanR0 <- 2.15 # cholera
cholera.maxR0 <- 18 # cholera

dengue <- read.csv('data/dengue_R0_historical.csv')
dengue <- subset(dengue, R0 < 30) # remove extreme outlier
dengue.meanR0 <- unname(quantile(dengue$R0, 0.5))
dengue.maxR0 <- round(max(dengue$R0))

# functions
da <- function(climateprob, outbreakprob, extremes){
  idx <- findInterval(climateprob, outbreakprob)
  r0id <- extremes[idx + 1]
  return(r0id)
}

far <- function(p0, p1){
  (p1 - p0)/p1
}

# should fix these for cholera
backfar <- function(FAR){
  p0 = unname(quantile(dengue$R0, 0.5))
  p1 = -p0/(FAR-1)
  return(p1)
}

backfar(0.1815844)

rr <- function(p0, p1){
  p1/p0
}

backrr <- function(RR){
  p0 = unname(quantile(dengue$R0, 0.5))
  p1 = RR * p0
  return(p1)
}
  
# backfar(0.83)
# backrr(0.189)

da_df <- function(meanr0, maxr0, x){
  # R0
  r0_extremes <- seq(0, maxr0, by = 0.1) # not sure if this should be 0 or meanr0
  r0_FAR <- far(p0 = meanr0, p1 = r0_extremes)
  r0_RR <- rr(p0 = meanr0, p1 = r0_extremes)
  
  # excess cases/deaths
  baselineCasesDeaths <- 100000
  excessCasesDeaths <- seq(baselineCasesDeaths, baselineCasesDeaths*10, by = 1000)
  excessFAR <- far(p0 = baselineCasesDeaths, p1 = excessCasesDeaths)
  excessRR <- far(p0 = baselineCasesDeaths, p1 = excessCasesDeaths)
  
  # climate data
  hist <- as.data.frame(x$prob_ee_hist)
  fut <- as.data.frame(x$prob_ee_fut)
  
  colnames(hist)[3:4] <- paste0('hist_', colnames(hist)[3:4])
  colnames(fut)[3:4] <- paste0('fut_', colnames(fut)[3:4])
  
  probs <- hist %>%
    left_join(fut) %>%
    mutate(FAR = (fut_prob - hist_prob)/fut_prob,
           RR = fut_prob/hist_prob)
  
  probs$r0_FAR <- da(climateprob = probs$FAR, outbreakprob = r0_FAR, extremes = r0_extremes)
  probs$r0_RR <- da(probs$RR, r0_RR, r0_extremes)
  
  probs <- na.omit(probs)  
  probs$excess_FAR <- da(probs$FAR, excessFAR, excessCasesDeaths)
  probs$excess_RR <- da(probs$RR, excessRR, excessCasesDeaths)
  
  # add classification
  probs$classification <- ifelse(round(probs$hist_prob, 2) >= 0.85 & round(probs$hist_prob, 2) <= 1.15, '1-yr event', NA)
  probs$classification[round(probs$hist_prob, 1) == 0.2] <- '5-yr event'
  probs$classification[round(probs$hist_prob, 1) == 0.1] <- '10-yr event'
  probs$classification[round(probs$hist_prob, 2) == 0.02] <- '50-yr event'
  probs$classification[round(probs$hist_prob, 2) == 0.01] <- '100-yr event'
  
  probs$classification <- factor(probs$classification, levels = c('1-yr event', '5-yr event', '10-yr event', '50-yr event', '100-yr event'))
  
  # add r0 probs
  # will need to update for cholera
  probs$R0_FAR_p1 <- backfar(probs$FAR)
  probs$R0_RR_p1 <- backrr(probs$RR)
  
  return(probs[, c('FAR', 'RR', 'r0_FAR', 'r0_RR', 'excess_FAR', 'excess_RR', 'classification', 'R0_FAR_p1', 'R0_RR_p1')])
}

# create new dataframe
df <- data.frame()

for(i in 1:length(site_data)){
  x <- readRDS(site_data[i])
  countryName <- gsub('../data/|precip_|temp_|metrics_|.RData', '', site_data[i])
  if(grepl('precip', site_data[i])){
    meanr0 = cholera.meanR0
    maxr0 = cholera.maxR0
    modelType = 'wbd'
  } else {
    meanr0 = dengue.meanR0
    maxr0 = dengue.maxR0
    modelType = 'vbd'
  }
  dfnew <- da_df(meanr0, maxr0, x)
  dfnew$Site <- countryName
  dfnew$ModelType <- modelType
  df <- rbind(df, dfnew)
}


# plots
library(ggplot2)

plotdir <- '../figures/analytical_solution/'

farplot <- ggplot(df, aes(x = FAR, y = r0_FAR, group = Site, color = ModelType, shape = Site)) +
  scale_shape_manual(values=1:length(unique(df$Site))) +
  scale_color_manual(values = c('vbd' = 'orange', 'wbd' = 'lightblue')) +
  geom_point() +
  theme_bw() +
  ylab('Min R0 to obtain equivalent FAR') +
  xlab('Climate FAR') #+
  xlim(0,1)
ggsave(farplot, file = paste0(plotdir, 'R0_FAR.pdf'))
  
rrplot <- ggplot(df, aes(x = RR, y = r0_RR, color = ModelType, shape = Site)) +
  scale_shape_manual(values=1:length(unique(df$Site))) +
  scale_color_manual(values = c('vbd' = 'orange', 'wbd' = 'lightblue')) +
  geom_point() +
  theme_bw() +
  ylab('Min R0 to obtain equivalent RR') +
  xlab('Climate RR') +
  geom_vline(xintercept = 1, linetype = 'dashed')

ggsave(rrplot, file = paste0(plotdir, 'R0_RR.pdf'))

# can divide y to be per capita
exfarplot <- ggplot(df, aes(x = FAR, y = excess_FAR/100000, color = ModelType, shape = Site)) +
  scale_shape_manual(values=1:length(unique(df$Site))) +
  scale_color_manual(values = c('vbd' = 'orange', 'wbd' = 'lightblue')) +
  geom_point() +
  theme_bw() +
  ylab('Min excess cases/deaths to obtain equivalent FAR\n(per 100,000 population)') +
  xlab('Climate FAR') +
  xlim(0,1)

ggsave(exfarplot, file = paste0(plotdir, 'Excess_FAR.pdf'))

exrrplot <- ggplot(df, aes(x = RR, y = excess_RR/100000, color = ModelType, shape = Site)) +
  scale_shape_manual(values=1:length(unique(df$Site))) +
  scale_color_manual(values = c('vbd' = 'orange', 'wbd' = 'lightblue')) +
  geom_point() +
  theme_bw() +
  ylab('Min excess cases/deaths to obtain equivalent RR\n(per 100,000 population)') +
  xlab('Climate RR') +
  xlim(0,1)

ggsave(exrrplot, file = paste0(plotdir, 'Excess_RR.pdf'))

dfa <- subset(df, !is.na(classification))
# dfa <- subset(df, )

classificationfar <- ggplot(dfa, aes(x = classification, y =  r0_FAR, fill = ModelType)) + 
  geom_boxplot() + 
  scale_fill_manual(values = c('vbd' = 'orange', 'wbd' = 'lightblue')) +
  theme_bw() +
  scale_y_continuous(minor_breaks = seq(0, 30, 1)) +
  xlab('Extreme event frequency') +
  ylab('R0 range to obtain equivalent FAR/RR')

ggsave(classificationfar, file = paste0(plotdir, 'Class_FAR.pdf'))

# ggplot(dfa, aes(x = classification, y =  r0_RR)) + geom_boxplot() + theme_bw() + facet_grid(~ModelType)

# relative p1s
dfb <- subset(df, ModelType == 'vbd')
ggplot(dfb, aes(x = FAR, y = R0_FAR_p1, group = Site, shape = Site)) +
  scale_shape_manual(values=1:length(unique(df$Site))) +
  scale_color_manual(values = c('vbd' = 'orange', 'wbd' = 'lightblue')) +
  geom_point() +
  theme_bw() +
  ylab('R0 extreme to reach equivalent FAR') +
  xlab('Climate FAR') +
  xlim(0,1) +
  ylim(0,2.5)

ggplot(dfb, aes(x = RR, y = R0_RR_p1, shape = Site)) +
  scale_shape_manual(values=1:length(unique(df$Site))) +
  scale_color_manual(values = c('vbd' = 'orange', 'wbd' = 'lightblue')) +
  geom_point() +
  theme_bw() +
  ylab('R0 extreme to reach equivalent RR') +
  xlab('Climate RR') +
  geom_vline(xintercept = 1, linetype = 'dashed') +
  geom_hline(yintercept = dengue.meanR0)

ggplot(dfa, aes(x = classification, y =  R0_RR_p1, fill = ModelType)) + 
  geom_boxplot() + 
  scale_fill_manual(values = c('vbd' = 'orange', 'wbd' = 'lightblue')) +
  theme_bw() +
  scale_y_continuous(minor_breaks = seq(0, 30, 1)) +
  xlab('Extreme event frequency') +
  ylab('R0 range to obtain equivalent FAR/RR')


