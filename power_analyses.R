# load libraries
library(tidyr)
library(pwrss)

# read in file
x <- readRDS('../data/long_summary.RData')

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
  pivot_longer(cols = c(final_size, mean_beta, beta_sd, Re_kurtosis, max_Re, max_Re_timing),
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

# save
saveRDS(reorganized_data, file = '../data/power_summary.RData')

