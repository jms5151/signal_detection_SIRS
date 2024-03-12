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

# list countries
vbd_countries <- c('Brazil', 'BurkinaFaso', 'Fiji', 'Italy', 'Pakistan', 'Philippines')
wbd_countries <- c('China', 'Ethiopia', 'Haiti', 'India', 'Sudan')

# Create separate parameter data frames for each model type and their associated window lengths
vbd_param_combinations <- expand.grid(
  country = vbd_countries,
  model_type = "vbd",
  window_length = c(90, NA)
)

wbd_param_combinations <- expand.grid(
  country = wbd_countries,
  model_type = "wbd",
  window_length = c(90, NA)
)

# Combine the parameter sets
param_combinations <- rbind(vbd_param_combinations, wbd_param_combinations)

# Register the cluster for parallel processing
cl <- makeCluster(detectCores(25))
registerDoParallel(cl)

# Now, use these combined parameter combinations in your parallel foreach loop
foreach (i = 1:nrow(param_combinations), .combine = 'c', .packages = c("dplyr")) %dopar% {
  # Extract parameters for this iteration
  params <- param_combinations[i, ]
  parallel_function(params$country, params$model_type, params$window_length)
}

stopCluster(cl)
