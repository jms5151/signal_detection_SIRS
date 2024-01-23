# Naming conventions
# ee = extreme event
# cc = climate change
# vbd = vector-borne disease
# wbd = waterborne disease
# temp = temperature
# rain = rainfall
# br = Brazil
# bg = Bangladesh
# drc = Democratic Republic of the Congo
# hi = Haiti

# source state variables and parameter values
source('state_variables.R')
source('parameter_values.R')

# source models
source('Lambrechts_model.R')
source('Eisenberg_model.R')
source('function_to_run_parallel_ode.R')

# Time span
timeSeq <- function(Yrs){seq(from = 1, to = 365 * Yrs, by = 1)}
ee.times <- timeSeq(Yrs = 3)
cc.times <- timeSeq(Yrs = 80) # 2020 - 2100

# simulate climate
source('simulate_climate.R')

# Starting conditions
vbd.start <- c(S = L_s, I = L_i, R = L_r)
wbd.start <- c(S = E_s, I = E_i, W = E_w, R = E_r)

# Run models

# Extreme event simulations
ee.vbd.br.sim <- runParallelODE(ee.vbd.br, vbd.start, ee.times, model_function = Lambrechts_model, vbd.params)
ee.vbd.bg.sim <- runParallelODE(ee.vbd.bg, vbd.start, ee.times, model_function = Lambrechts_model, vbd.params)

ee.wbd.drc.sim <- runParallelODE(ee.wbd.drc, wbd.start, ee.times, model_function = Eisenberg_model, wbd.params)
ee.wbd.hi.sim <- runParallelODE(ee.wbd.hi, wbd.start, ee.times, model_function = Eisenberg_model, wbd.params)

# climate change simulations
# cc.vbd.br.sim <- runParallelODE(cc.vbd.br, vbd.start, cc.times, model_function = Lambrechts_model, vbd.params)
# cc.vbd.bg.sim <- runParallelODE(cc.vbd.bg, vbd.start, cc.times, model_function = Lambrechts_model, vbd.params)
# cc.wbd.drc.sim <- runParallelODE(cc.wbd.drc, wbd.start, cc.times, model_function = Eisenberg_model, wbd.params)
# cc.wbd.hi.sim <- runParallelODE(cc.wbd.hi, wbd.start, cc.times, model_function = Eisenberg_model, wbd.params)
