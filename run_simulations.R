# source state variables and parameter values
source('state_variables.R')
source('parameter_values.R')

# source models
source('Lambrechts_model.R')
source('Eisenberg_model.R')

# source functions to run models
source('function_to_run_parallel_ode.R')
source('functions_to_calculate_beta.R')

# time span
source('time_spans.R')

# Starting conditions
vbd.start <- c(S = s, I = i, R = r)
wbd.start <- c(S = s, I = i, W = w, R = r)

# Run models

# Extreme event simulations
ee.vbd.br.sim <- runParallelODE(ee.vbd.br, vbd.start, ee.times, model_function = Lambrechts_model, vbd.params)
ee.vbd.bg.sim <- runParallelODE(ee.vbd.bg, vbd.start, ee.times, model_function = Lambrechts_model, vbd.params)

ee.wbd.drc.sim <- runParallelODE(ee.wbd.drc, wbd.start, ee.times, model_function = Eisenberg_model, wbd.params)
ee.wbd.hi.sim <- runParallelODE(ee.wbd.hi, wbd.start, ee.times, model_function = Eisenberg_model, wbd.params)

test <- runParallelODE(beta_list = ee2, wbd.start, ee.times, model_function = Eisenberg_model, wbd.params)
plot.ts(test$iter1_5I_1D_post$I, col = 'red')
lines(test$iter5_normal$I)

ee_betas <- readRDS('../data/Haiti_beta_ts.RData')
ee2 <- ee_betas[c(1:5, 4500:4505)]
test2 <- runParallelODE(beta_list = ee2, vbd.start, ee.times, model_function = Lambrechts_model, vbd.params)
plot.ts(test2$iter1_100I_15D_post$I, col = 'red')
lines(test2$iter1_normal$I)
# analyze ee simulations

# climate change simulations
# cc.vbd.br.sim <- runParallelODE(cc.vbd.br, vbd.start, cc.times, model_function = Lambrechts_model, vbd.params)
# cc.vbd.bg.sim <- runParallelODE(cc.vbd.bg, vbd.start, cc.times, model_function = Lambrechts_model, vbd.params)
# cc.wbd.drc.sim <- runParallelODE(cc.wbd.drc, wbd.start, cc.times, model_function = Eisenberg_model, wbd.params)
# cc.wbd.hi.sim <- runParallelODE(cc.wbd.hi, wbd.start, cc.times, model_function = Eisenberg_model, wbd.params)
