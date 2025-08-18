# simulate climate time series
source('functions_to_simulate_climate.R')
source('time_spans.R')

savedir <- '../data/sim_climate/'

sim_climate_by_regime <- function(xmin, xmax, regimeType, years){
  s <- simulate_seasonal_climate(xmin = xmin, xmax = xmax, years = Yrs)
  saveRDS(s, paste0(savedir, 'normal_', regimeType, '_clim.RData'))
  return(s)
}

sin_temp <- sim_climate_by_regime(xmin = 15, xmax = 25, regimeType = 'temperate', years = Yrs)
sin_warm <- sim_climate_by_regime(xmin = 20, xmax = 30, regimeType = 'warm', years = Yrs)
sin_hot <- sim_climate_by_regime(xmin = 25, xmax = 35, regimeType = 'hot', years = Yrs)

sin_dry <- sim_climate_by_regime(xmin = 20, xmax = 60, regimeType = 'dry', years = Yrs)
sin_mod <- sim_climate_by_regime(xmin = 50, xmax = 150, regimeType = 'moderate', years = Yrs)
sin_wet <- sim_climate_by_regime(xmin = 75, xmax = 500, regimeType = 'wet', years = Yrs)