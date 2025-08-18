# post-processing

# load libraries
library(dplyr)
library(tidyr)
library(pracma)
library(stringr)

# source file path and parameter values
# source('filepaths.R')
scratch_path <- '../data/sim_results_ee/'
source('functions_for_analysis.R')

eefiles <- list.files(scratch_path)
eefiles <- eefiles[grepl('S_', eefiles)]

# regime types
wbd.regimes <- 'dry|moderate|wet'
vbd.regimes <- 'temperate|warm|hot'

# create empty data frame to store results
df <- data.frame()

for(i in 1:length(eefiles)){
  # read in ee outputs
  filePath <- paste0(scratch_path, eefiles[i])
  x <- readRDS(filePath)
  # read in normal outputs
  regime <- gsub('S_|min_|max_|t_|.RData', '', eefiles[i])
  normalFilepath <- paste0(gsub('_ee', '', scratch_path), 'normal_', regime, '.RData')
  normals <- readRDS(normalFilepath)
  if(grepl(wbd.regimes, filePath)){
    modType <- 'wbd'
  } else {
    modType <- 'vbd'
  }
  eeMetrics <- process_sir_output(datalist = x, model_type = modType)
  cnMetrics <- process_sir_output(datalist = normals, model_type = modType)
  newdf <- rbind(eeMetrics, cnMetrics)
  newdf$List <- ifelse(newdf$List == '1', paste0(regime, '_control'), newdf$List)
  newdf$suscept <- ifelse(grepl('min', eefiles[i]), 'S_min', 'S_max')
  df <- rbind(df, newdf)
}

# normalize data
df$regime <- gsub('_.*', '', df$List)
normalized_df <- df

# format control
normalized_df$List <- gsub('.*_control', 'control', normalized_df$List)
normalized_df$List <- gsub('dry_|moderate_|wet_|temperate_|warm_|hot_', '', normalized_df$List)

# combine across metrics
wbd_group <- normalized_df[grepl('dry|moderate|wet', normalized_df$regime), ]
vbd_group <- normalized_df[grepl('temperate|warm|hot', normalized_df$regime), ]

metricNames <- c('peak_timing', 'cumulative_proportion', 'outbreak_duration')

spread_by_metric <- function(df, metric){
  ndf2 <- df[c('List', 'regime', 'suscept', metric)] %>%
    spread(key = List, value = metric)
  return(ndf2)
}

# calculate differences by row
calc_perc_change <- function(df, annualized=F){
  perturbationColumns <- colnames(df)[!grepl(c('List|regime|suscept|control'), colnames(df))]
  if(annualized == T){
    df[perturbationColumns] <- (df[perturbationColumns]-df$control)/365
  } else {
    df[perturbationColumns] <- (df[perturbationColumns]-df$control)/df$control
  }
  return(df)
}

process_group <- function(dfx){
  new_df <- data.frame()
  for(i in 1:length(metricNames)){
    df1 <- spread_by_metric(df = dfx, metric = metricNames[i])
    if(metricNames[i] == 'peak_timing'){
      df2 <- calc_perc_change(df = df1, annualized = T)
    } else {
      df2 <- calc_perc_change(df = df1)
    }
    df3 <- gather_by_metric(df = df2, metric = metricNames[i])
    if(i == 1){
      new_df <- df3
    } else {
      new_df <- left_join(new_df, df3)
    }
    cat(i, '\n')
  }
  return(new_df)
}

# process and combine
wbd_diff <- process_group(dfx = wbd_group)
vbd_diff <- process_group(df = vbd_group)
longCombined <- rbind(wbd_diff, vbd_diff)
longCombined[is.na(longCombined)] <- 0

# format columns
longCombined$regime <- str_to_sentence(longCombined$regime)
longCombined$intensity <- sapply(strsplit(longCombined$ee, "_"), `[`, 1)
longCombined$intensity <- gsub('I', '', longCombined$intensity)
longCombined$intensity <- as.numeric(longCombined$intensity)
longCombined$duration <- sapply(strsplit(longCombined$ee, "_"), `[`, 2)
longCombined$duration <- gsub('D', '', longCombined$duration)
longCombined$duration <- as.numeric(longCombined$duration)

# save
saveRDS(object = longCombined, '../data/sim_summaries/long_summary.RData')
