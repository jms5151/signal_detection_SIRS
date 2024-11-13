# Load required libraries
library(GGally)
library(ggplot2)
library(tidyverse)

# read in data
x <- readRDS('../data/sim_summaries/long_summary.RData')

# format
x$Dataset <- ifelse(
  grepl('^normal', x$List),
  gsub("^[^_]*_[^_]*_([^_]*)_?.*", "\\1", x$List),
  gsub("^[^_]*_([^_]*)_.*", "\\1", x$List)
)

x$ee <- ifelse(x$experiment == 'control', 'control', x$ee)

# remove duplicted rows (controls were duplicted for power analysis)
x <- x[!duplicated(x), ]

# Normalize metrics by regime, susceptibility start
normalized_df <- x %>% 
  group_by(filename, ee) %>%
  mutate(
    peak_timing = as.vector(scale(peak_timing)),
    max_incidence = as.vector(scale(max_incidence)),
    cumulative_proportion = as.vector(scale(cumulative_proportion)),
    duration = as.vector(scale(duration))#,
    # peakiness = scale(peakiness)
    ) %>%
  as.data.frame()

# restructure from long to wide (columns = ids, control, scenarios)
spread_by_metric <- function(df, metric){
  ndf2 <- df[c('Dataset', 'filename', 'ee', metric)] %>%
    spread(key = ee, value = metric)
  return(ndf2)
}

t1 <- subset(normalized_df, filename == 'S_max_t_hot')
test <- spread_by_metric(df = t1, metric = 'duration')

# calculate squared differences
calc_sq_diff <- function(df){
  perturbationColumns <- colnames(df)[!grepl('Dataset|filename|control', colnames(df))]
  df[perturbationColumns] <- (df[perturbationColumns]-df$control)^2
  return(df)
}

test2 <- calc_sq_diff(df = test)

# may need to go from wide to long again for plotting purposes
gather_by_metric <- function(df, metric){
  df2 <- df %>%
    select(-control) %>%
    gather(key = ee, value = squared_diff, -c(Dataset, filename))
  colnames(df2)[ncol(df2)] <- paste0(metric, '_squared_diff')
  return(df2)
}

test3 <- gather_by_metric(df = test2, metric = 'testmetric')

# combine across metrics
wbd_group <- normalized_df[grepl('dry|moderate|wet', normalized_df$filename), ]
vbd_group <- normalized_df[grepl('temperate|warm|hot', normalized_df$filename), ]

metricNames <- c('peak_timing', 'max_incidence', 'cumulative_proportion', 'duration')

process_group <- function(df){
  for(i in 1:length(metricNames)){
    df1 <- spread_by_metric(df = df, metric = metricNames[i])
    df2 <- calc_sq_diff(df = df1)
    df3 <- gather_by_metric(df = df2, metric = metricNames[i])
    if(i == 1){
      new_df <- df3
    } else {
      new_df <- left_join(new_df, df3)
    }
  }
  return(new_df)
}

wbd_sq_diff <- process_group(df = wbd_group)
vbd_sq_diff <- process_group(df = vbd_group)

# for plotting
longCombined <- rbind(wbd_sq_diff, vbd_sq_diff)

# subgroups
s_groups <- unique(longCombined$filename)

# pair plot function
create_plots <- function(sName){
  # subset data
  xx <- subset(longCombined, filename == sName)
  # pair plots
  p <- ggpairs(xx[,4:7]) + theme_bw() + ggtitle(sName)
  return(p)
}

# create and save all pair plots
# This isn't exactly correct, what we want is the difference!
for(j in s_groups){
    p <- create_plots(sName = j)
    ggsave(filename = paste0('../figures/pair_plots/', j, '.pdf'), plot = p, width = 9, height = 6)
}


