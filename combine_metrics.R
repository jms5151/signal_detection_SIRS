# Load required libraries
library(GGally)
library(tidyverse)
library(stringr)
library(ggplot2)
library(patchwork)

# read in data
x <- readRDS('../data/sim_summaries/long_summary.RData')

# Normalize metrics by regime, susceptibility start
normalized_df <- x %>% 
  mutate(
    peak_timing = as.vector(scale(peak_timing))
    , cumulative_proportion = as.vector(scale(cumulative_proportion))
    , outbreak_duration = as.vector(scale(outbreak_duration))
  ) %>%
  as.data.frame()

# restructure from long to wide (columns = ids, control, scenarios)
idvars <- c('List', 'suscept', 'regime', 'intensity', 'duration', 'experiment')

spread_by_metric <- function(df, metric){
  ndf2 <- df[c(idvars, metric)] %>%
    spread(key = List, value = metric)
  return(ndf2)
}

# calculate differences by row
calc_diff <- function(df){
  perturbationColumns <- colnames(df)[!grepl(c(idvars, 'control'), colnames(df))]
  df[perturbationColumns] <- df[perturbationColumns]-df$control
  return(df)
}

# may need to go from wide to long again for plotting purposes
gather_by_metric <- function(df, metric){
  df2 <- df %>%
    select(-control) %>%
    gather(key = ee, value = diff, -idvars)
  colnames(df2)[ncol(df2)] <- paste0(metric, '_diff')
  return(df2)
}

# combine across metrics
wbd_group <- normalized_df[grepl('dry|moderate|wet', normalized_df$filename), ]
vbd_group <- normalized_df[grepl('temperate|warm|hot', normalized_df$filename), ]

metricNames <- c('peak_timing', 'cumulative_proportion', 'duration')

process_group <- function(df){
  for(i in 1:length(metricNames)){
    df1 <- spread_by_metric(df = df, metric = metricNames[i])
    df2 <- calc_diff(df = df1)
    df3 <- gather_by_metric(df = df2, metric = metricNames[i])
    if(i == 1){
      new_df <- df3
    } else {
      new_df <- left_join(new_df, df3)
    }
  }
  return(new_df)
}

wbd_diff <- process_group(df = wbd_group)
vbd_diff <- process_group(df = vbd_group)

# for plotting
longCombined <- rbind(wbd_diff, vbd_diff)

# format numeric columns
longCombined[paste0(metricNames, '_diff')] <- lapply(longCombined[paste0(metricNames, '_diff')], as.numeric)

# Define a normalization function that shifts and scales values
normalize_shift <- function(x) {
  shifted <- x - min(x)  # Shift to make all values positive
  shifted / max(shifted)  # Normalize to [0, 1]
}

# Apply normalization and map to RGB channels
data <- longCombined %>%
  mutate(
    # Normalize and shift each metric
    peak_timing_norm = normalize_shift(peak_timing_diff),
    cumulative_proportion_norm = normalize_shift(cumulative_proportion_diff),
    duration_norm = normalize_shift(duration_diff),
    
    # Map to RGB channels
    R = peak_timing_norm,  # Red channel
    G = cumulative_proportion_norm,  # Green channel
    B = duration_norm,  # Blue channel
    
    # Optionally track sign to overlay
    above_zero = ifelse(peak_timing_diff > 0 & cumulative_proportion_diff > 0 & duration_diff > 0, TRUE, FALSE)
  )

# Combine into hex color
data <- data %>%
  mutate(color = rgb(R, G, B, maxColorValue = 1))


# save data

# ---------------------- NEw test end
