# load libraries
library(tidyr)
library(pwrss)

# read in file
x <- readRDS('../data/sim_summaries/long_summary.RData')

# function to calculate power
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

# reorganize data
reorganized_data <- x %>%
  pivot_longer(cols = c(cumulative_cases, max_S, peak_timing_cases, max_cases, Peakiness),
               names_to = "metric",
               values_to = "value") %>%
  group_by(metric, experiment, filename, ee) %>%
  summarize(
    mean_value = mean(value),
    sd_value = sd(value),
    n = length(value)
  ) %>%
  pivot_wider(names_from = experiment,
              values_from = c(mean_value, sd_value, n),
              names_sep = "_") %>%
  as.data.frame()

# calculate power
reorganized_data$power <- mapply(calc_power, 
                                 reorganized_data$mean_value_control
                                 , reorganized_data$mean_value_experiment
                                 , reorganized_data$sd_value_control
                                 , reorganized_data$sd_value_experiment
                                 , reorganized_data$n_control
                                 , reorganized_data$n_experiment)

# add some useful labels
reorganized_data$suscept <- gsub('_t.*', '', reorganized_data$filename)
reorganized_data$type <- ifelse(grepl('multi', reorganized_data$filename)==T, 'multiplicative', 'additive')
reorganized_data$regime <- gsub('.*_t_', '', reorganized_data$filename)
reorganized_data$regime <- gsub('multi_', '', reorganized_data$regime)
reorganized_data$intensity <- gsub('_.*', '', reorganized_data$ee)
reorganized_data$intensity <- gsub('I', '', reorganized_data$intensity)
reorganized_data$intensity <- gsub('neg', '-', reorganized_data$intensity)
reorganized_data$intensity <- as.numeric(reorganized_data$intensity)
reorganized_data$duration <- gsub('.*_', '', reorganized_data$ee)
reorganized_data$duration <- gsub('D', '', reorganized_data$duration)
reorganized_data$duration <- as.numeric(reorganized_data$duration)

# save
saveRDS(reorganized_data, file = '../data/sim_summaries/power_summary.RData')

# reorganize to determine most robust metric
highest_power <- reorganized_data %>%
  group_by(filename, ee) %>%
  top_n(1, power) %>%
  # select(filename, ee, metric, power)
  select(filename, ee, suscept, regime, type, intensity, duration, metric, power) %>%
  as.data.frame()

# save
saveRDS(highest_power, file = '../data/sim_summaries/highest_power_summary.RData')
