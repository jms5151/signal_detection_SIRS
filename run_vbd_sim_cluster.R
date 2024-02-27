# source state variables and parameter values
source('state_variables.R')
source('parameter_values.R')

# source model
source('Lambrechts_model.R')
source('function_to_run_parallel_ode.R')

# Time span
source('time_spans.R')

# filepath
source('filepaths.R')

# Starting conditions
vbd.start <- c(S = L_s, I = L_i, R = L_r)

# Run models
args = commandArgs(trailingOnly = TRUE)

inputFilePath <- paste0(scratch_path, 'data/ee_vbd_', args, '.RData')
INPUT <- readRDS(inputFilePath)

# Extreme event simulations
#OUTPUT <- print(inputFilePath)
OUTPUT <- runParallelODE(INPUT, vbd.start, ee.times, model_function = Lambrechts_model, vbd.params)

# save results
outputFilePath <- paste0(scratch_path, 'results/ee_vbd_', args, '.RData')
saveRDS(OUTPUT, file = outputFilePath)