# post-processing

# load libraries
library(moments)
library(dplyr)
library(tidyr)
library(doParallel)
library(foreach)
library(pracma)

# source file path and parameter values
source('filepaths.R')
source('functions_for_analysis.R')

eefiles <- list.files(paste0(scratch_path, '/results/'))
eefiles <- eefiles[grepl('S_', eefiles)]

df <- data.frame()
for(i in 1:length(eefiles)){
  # read in ee outputs
  filePath <- paste0(scratch_path, 'results/', eefiles[i])
  x <- readRDS(filePath)
  # read in normal outputs
  regime <- gsub('S_|min_|max_|multi_|t_|.RData', '', eefiles[i])
  normalFilepath <- paste0(scratch_path, 'results/normal_', regime, '.RData')
  normals <- readRDS(normalFilepath)
  # replicate normal outputs for comparison with ee outputs
  repeatN <- length(x)/length(normals)
  repeated_list <- rep(list(normals), times = repeatN)
  normal_rep <- unlist(repeated_list, recursive = FALSE)
  # compute metrics
  if(grepl('dry|moderate|wet', regime)){
    modType <- 'wbd'
  } else {
    modType <- 'vbd'
  }
  eeMetrics <- process_sir_output(datalist = x, model_type = modType, explabel = 'experiment')
  cnMetrics <- process_sir_output(datalist = normal_rep, model_type = modType, explabel = 'control')
  newdf <- rbind(eeMetrics, cnMetrics)
  newdf$ee <- rep(eeMetrics$List, 2)
  newdf$filename <- gsub('.RData', '', eefiles[i])
  df <- rbind(df, newdf)
}

# update ee column
df$ee <- gsub('^(?:[^_]*_){2}', '', df$ee)

# save
saveRDS(df, paste0(scratch_path, 'summaries/long_summary.RData'))
