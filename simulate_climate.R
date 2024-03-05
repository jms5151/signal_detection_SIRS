# extreme events simulations
source('functions_to_simulate_climate.R')
source('time_spans.R')

# Global conditions
t.Magnitudes = seq(1, 10, 1) # 10
t.Durations = seq(1, 40, 1) # 40
r.Magnitudes = seq(5, 100, 5) # 100
r.Durations = seq(1, 15, 1)  # 15
ExEV_times = c('pre', 'peak', 'post')
Iterations = 5

# extreme event occurs in second year of simulation
t1 = 365
t2 = 365 * 2

# list countries
vbd_countries <- c('Brazil', 'BurkinaFaso', 'Fiji', 'Italy', 'Pakistan', 'Philippines')
wbd_countries <- c('China', 'Ethiopia', 'Haiti', 'India', 'Sudan')

# list data
site_data <- list.files('../data/', full.names = T)
site_data <- site_data[grepl('climate_metrics_', site_data)]

# run loop to generate time series data for model input & analysis for all countries/climate regimes
for(i in site_data){
  # read in data
  df <- readRDS(i)
  df <- df$base_metrics
  
  # name country
  country <- gsub('../data/climate_metrics_|.RData', '', i)
  
  # determine seasonality 
  if(country == 'Haiti'){
    slength = 2
  } else {
    slength = 1
  }
  
  # assign global conditions and function to translate climate into transmission rate
  if(length(grep(country, wbd_countries))>0){
    mag = r.Magnitudes
    dur = r.Durations
    betafun = Eisenberg_beta
  } else {
    mag = t.Magnitudes
    dur = t.Durations
    betafun = Lambrechts_beta
  }

  # generate climate time series
  ee <- ee_list(
    x = df
    , s = slength
    , years = years
    , iter = Iterations
    , magnitudes = mag
    , durations = dur
    , timing = ExEV_times
    , time1 = t1
    , time2 = t2
  )
  
  # save start time of perturbation
  ee_start_times <- lapply(ee, function(x) x[[2]])
  ee_start_times <- ee_start_times[grepl('normal', names(ee_start_times))==F]
  startFileName <- gsub('climate_metrics_', 'ee_start_time_', i)
  saveRDS(ee_start_times, file = startFileName)
          
  # save climate time series data
  ee2  <- lapply(ee[!grepl('normal', names(ee))], function(x) x[[1]])
  ee2 <- c(ee[grepl('normal', names(ee))], ee2)
  climFileName <- paste0('../data/climate_ts_', country, '.RData')
  saveRDS(ee2, file = climFileName)
  
  # translate climate time series to beta values
  ee_betas <- lapply(ee2, function(x) betafun(x))
  betaFileName <- paste0('../data/', country, '_beta_ts.RData')
  saveRDS(ee_betas, file = betaFileName)
}