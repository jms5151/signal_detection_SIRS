# post-processing

# load libraries
library(tidyr)
library(pracma)
library(stringr)

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
  if(grepl('dry|moderate|wet', regime)){
    modType <- 'wbd'
  } else {
    modType <- 'vbd'
  }
  eeMetrics <- process_sir_output(datalist = x, model_type = modType, explabel = 'experiment')
  cnMetrics <- process_sir_output(datalist = normals, model_type = modType, explabel = 'control')
  newdf <- rbind(eeMetrics, cnMetrics)
  newdf$filename <- gsub('.RData', '', eefiles[i])
  df <- rbind(df, newdf)
}

# format columns
df$suscept <- strsplit(df$filename, '_')[[1]][2]
df$regime <- strsplit(df$filename, '_')[[1]][4]
df$regime <- str_to_sentence(df$regime)
df$intensity <- strsplit(df$List, '_')[[1]][2]
df$intensity <- gsub('I', '', df$intensity)
df$intensity <- as.numeric(df$intensity)
df$duration <- strsplit(df$List, '_')[[1]][3]
df$duration <- gsub('D', '', df$duration)
df$duration <- as.numeric(df$duration)

# save
saveRDS(df, paste0(scratch_path, 'summaries/long_summary.RData'))
