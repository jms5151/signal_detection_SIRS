# simulate climate time series
source('ancillary_climate_data.R')
source('functions_to_simulate_climate.R')
source('time_spans.R')

savedir <- '../data/sim_climate/'

sim_climate_from_data <- function(df, regimeType){
  s <- simulate_seasonal_climate(xmin = mean(df$Min_Month_Value), xmax = mean(df$Max_Month_Value), years = Yrs)
  saveRDS(s, paste0(savedir, 'normal_', regimeType, '_clim.RData'))
  return(s)
}

sin_temp <- sim_climate_from_data(df = t20, regimeType = 'temperate') 
sin_warm <- sim_climate_from_data(df = t24, regimeType = 'warm')
sin_hot <- sim_climate_from_data(df = t28, regimeType = 'hot')

sin_dry <- sim_climate_from_data(df = dry, regimeType = 'dry')
sin_mod <- sim_climate_from_data(df = mod, regimeType = 'moderate')
sin_wet <- sim_climate_from_data(df = wet, regimeType = 'wet')
