# post-processing

# load libraries
library(pracma) # trapz
library(moments)
library(dplyr)
library(tidyr)
library(doParallel)
library(foreach)

# source file path and parameter values
source('filepaths.R')
source('parameter_values.R')
source('functions_for_analysis.R')

eefiles <- list.files(paste0(scratch_path, '/results/'))
eefiles <- eefiles[grepl('S_', eefiles)]

df <- data.frame()
for(i in 1:length(eefiles)){
  # read in ee outputs
  filePath <- paste0(scratch_path, 'results/', eefiles[i])
  x <- readRDS(filePath)
  # read in ee start times 
  startTimesFilepath <- gsub('results', 'start_times', filePath)
  startTimesFilepath <- gsub('multi_', '', startTimesFilepath) # remove 'multi_' if in filename
  sTime <- readRDS(startTimesFilepath)
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
    modParams <- wbd.params
  } else {
    modType <- 'vbd'
    modParams <- vbd.params
  }
  eeMetrics <- process_sir_output(datalist = x, start_list = sTime, model_type = modType, params = modParams, explabel = 'experiment')
  cnMetrics <- process_sir_output(datalist = normal_rep, start_list = sTime, model_type = modType, params = modParams, explabel = 'control')
  newdf <- rbind(eeMetrics, cnMetrics)
  newdf$filename <- gsub('.RData', '', eefiles[i])
  df <- rbind(df, newdf)
}