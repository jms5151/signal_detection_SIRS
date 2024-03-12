source('filepaths.R')

summaryFiles <- list.files('../data/results/', full.names = T)
summaryFiles <- summaryFiles[grepl('short', summaryFiles)]

# Load the tidyr package
library(tidyverse)
# library(tidyr)
library(stringr)
library(pwrss)


calc_power <- function(mean1, mean2, sd1, sd2, n1, n2){
  p <- pwrss.t.2means(mu1 = mean1
                      , mu2 = mean2
                      , sd1 = sd1
                      , sd2 = sd2
                      , kappa = n1/n2 # kappa = sample size 1 / sample size 2
                      , n2 = n2
                      , alpha = 0.05
                      , alternative = "not equal")
  return(p$power)  
}


countries <- c('Brazil', 'BurkinaFaso', 'Fiji', 'Italy', 'Pakistan', 'Philippines', 'China', 'Ethiopia', 'Haiti', 'India', 'Sudan')

summary_df <- c()

# combine summary data
for(i in summaryFiles){
  # read file
  results_flattened <- readRDS(i)
  
  # summarize return time and correlation variables
  results_flattened$Return_time[results_flattened$Return_time == 99999] <- NA
  
  returnAndCorr <- results_flattened %>%
    group_by(Dataset) %>%
    summarise(Return_time = mean(Return_time, na.rm = T)
              , CE_correlation = mean(CE_Correlation)) %>%
    pivot_longer(cols = c(Return_time, CE_correlation)
                 , names_to = "Variable"
                 , values_to = "Value")
  
  # format data for power analyses
  long_dataset <- results_flattened %>%
    pivot_longer(
      cols = -c(Dataset), # Keeps 'Dataset' column as is
      names_to = c(".value", "Variable"), # Directs how to transform and name parts of the column names
      names_pattern = "(Control|Experiment)_(.+)", # Regular expression to match and split the column names
      values_to = c("Control", "Experiment") # Names for the reshaped value columns
    ) %>%
    na.omit()
  
  long_dataset <- long_dataset %>%
    group_by(Dataset, Variable) %>%
    summarise(mu_control = mean(Control)
              , sd_control = sd(Control)
              , mu_experiment = mean(Experiment)
              , sd_experiment = sd(Experiment)
              , n_control = length(Control)
              , n_experiment = length(Experiment)) 
  
  long_dataset$Value <- mapply(calc_power, long_dataset$mu_control, long_dataset$mu_experiment, long_dataset$sd_control, long_dataset$sd_experiment, long_dataset$n_control, long_dataset$n_experiment)
  
  # combine dataframes
  df <- rbind(long_dataset[,c('Dataset', 'Variable', 'Value')], returnAndCorr)
  df <- as.data.frame(df)
  
  # add magnitude, duration, event timing
  split_columns <- as.data.frame(str_split_fixed(df$Dataset, "_", n = Inf))
  
  df$magnitude <- split_columns$V1
  df$magnitude <- gsub('I', '', df$magnitude)
  df$magnitude <- as.numeric(df$magnitude)
  
  df$duration <- split_columns$V2
  df$duration <- gsub('D', '', df$duration)
  df$duration <- as.numeric(df$duration)
  
  df$ee_timing <- split_columns$V3
  
  # add identifiers
  df$diseaseSystem <- ifelse(grepl('vbd', i), 'vbd', 'wbd')
  df$timeWindow <- ifelse(grepl('short', i), 'short', 'long')
  df$region <- countries[str_detect(i, countries)]
  
  # row bind data
  summary_df <- rbind(summary_df, df)
}

saveRDS(summary_df, file = '../data/ee_summary_all.RData')


