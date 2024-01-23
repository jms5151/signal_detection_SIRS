# extreme events simulations

source('functions_to_simulate_climate.R')

# short term, extreme events
# Global conditions
load('data/historical_extremes.RData') # reference values from historical datasets
t.Magnitudes = seq(1, 20, 1)
t.Durations = seq(1, 15, 1)
r.Magnitudes = seq(10, 200, 10)
r.Durations = seq(1, 10, 1) 
ExEV_times = c('pre', 'peak', 'post')
Iterations = 2

# extreme event occurs in second year of simulation
t1 = 365
t2 = 365 * 2

# Local conditions calculated from weatherunderground data
# from source('ancillary_climate_data.R')
load('data/historical_temperature_values.RData')
load('data/historical_rainfall_drc.RData')
load('data/historical_rainfall_hi.RData')

# Generate extreme event time series
ee.vbd.bg <- ee_list(
  climType = 'temperature'
  , times = ee.times
  , iter = Iterations
  , var1 = historical.temperature.values$mean[historical.temperature.values$loc == 'bg']
  , var2 = historical.temperature.values$amp[historical.temperature.values$loc == 'bg']
  , var3 = historical.temperature.values$sd[historical.temperature.values$loc == 'bg']
  , magnitudes = t.Magnitudes
  , durations = t.Durations
  , timing = ExEV_times
  , time1 = t1
  , time2 = t2
)

ee.vbd.br <- ee_list(
  climType = 'temperature'
  , times = ee.times
  , iter = Iterations
  , var1 = historical.temperature.values$mean[historical.temperature.values$loc == 'br']
  , var2 = historical.temperature.values$amp[historical.temperature.values$loc == 'br']
  , var3 = historical.temperature.values$sd[historical.temperature.values$loc == 'br']
  , magnitudes = t.Magnitudes
  , durations = t.Durations
  , timing = ExEV_times
  , time1 = t1
  , time2 = t2
)

ee.wbd.drc <- ee_list(
  climType = 'rainfall'
  , times = ee.times
  , iter = Iterations
  , var1 = round(monthly.R.var.drc$rainy_days)
  , var2 = monthly.R.var.drc$total_rainfall
  , var3 = NA
  , magnitudes = r.Magnitudes
  , durations = r.Durations
  , timing = 'peak'
  , time1 = t1
  , time2 = t2
)

ee.wbd.hi <- ee_list(
  climType = 'rainfall'
  , times = ee.times
  , iter = Iterations
  , var1 = round(monthly.R.var.hi$rainy_days)
  , var2 = monthly.R.var.hi$total_rainfall
  , var3 = NA
  , magnitudes = r.Magnitudes
  , durations = r.Durations
  , timing = 'peak'
  , time1 = t1
  , time2 = t2
)

# generate climate change time series
# cc.vbd.br <- cc_list()
# cc.vbd.bg <- cc_list()
# cc.wbd.drc <- cc_list()
# cc.wbd.hi <- cc_list()