# source state variables and parameter values
source('state_variables.R')
source('parameter_values.R')

# source models
source('Lambrechts_model.R')

# source functions to run models
source('function_to_run_parallel_ode.R')

# time span
source('time_spans.R')

# Starting conditions
vbd.start <- c(S = s, I = i, R = r)

# filepath
source('filepaths.R')

# Run models
args = commandArgs(trailingOnly = TRUE)

inputFilePath <- paste0(scratch_path, 'data/', args, '_betas.RData')
INPUT <- readRDS(inputFilePath)

# Extreme event simulations
#OUTPUT <- print(inputFilePath)
INPUT <- readRDS(inputFilePath)
OUTPUT <- runParallelODE(INPUT, vbd.start, ee.times, model_function = Lambrechts_model, vbd.params)

# save results
outputFilePath <- paste0(scratch_path, 'results/', args, '.RData')
saveRDS(OUTPUT, file = outputFilePath)
