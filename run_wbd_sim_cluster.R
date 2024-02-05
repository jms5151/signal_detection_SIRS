# source state variables and parameter values
source('state_variables.R')
source('parameter_values.R')

# source model
source('Eisenberg_model.R')
source('function_to_run_parallel_ode.R')

# Time span
source('time_spans.R')

# filepath
source('filepaths.R')

# Starting conditions
wbd.start <- c(S = E_s, I = E_i, W = E_w, R = E_r)

# Run models
args = commandArgs(trailingOnly = TRUE)

inputFilePath <- paste0(scratch_path, 'data/ee_wbd_', args, '.RData')
INPUT <- readRDS(inputFilePath)

# Extreme event simulations
#OUTPUT <- inputFilePath
OUTPUT <- runParallelODE(INPUT, wbd.start, ee.times, model_function = Eisenberg_model, wbd.params)

# save results
outputFilePath <- paste0(scratch_path, 'results/ee_wbd_', args, '.RData')
saveRDS(OUTPUT, file = outputFilePath)