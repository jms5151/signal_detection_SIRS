# extreme events simulations
source('functions_to_simulate_climate.R')
source('time_spans.R')
source('functions_to_calculate_beta.R')

# Global conditions
t.Magnitudes = seq(1, 10, 1) # 10
t.Durations = seq(1, 40, 1) # 40
r.Magnitudes = seq(5, 100, 5) # 100
r.Durations = seq(1, 15, 1)  # 15
ExEV_times = c('pre', 'peak', 'post')
Iterations = 50

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
    , years = Yrs
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
  startFileName <- gsub('climate_metrics_', 'ee_start_times/ee_start_time_', i)
  saveRDS(ee_start_times, file = startFileName)
          
  # save climate time series data
  ee2  <- lapply(ee[!grepl('normal', names(ee))], function(x) x[[1]])
  ee2 <- c(ee[grepl('normal', names(ee))], ee2)
  climFileName <- paste0('../data/climate_ts/climate_ts_', country, '.RData')
  saveRDS(ee2, file = climFileName)
  
  # translate climate time series to beta values
  ee_betas <- lapply(ee2, function(sub_list) {
    unlist(lapply(sub_list, betafun))
  })
  
  betaFileName <- paste0('../data/beta_ts/', country, '_beta_ts.RData')
  saveRDS(ee_betas, file = betaFileName)
}

### new climate time series
source('ancillary_climate_data.R')
source('functions_to_simulate_climate.R')
source('time_spans.R')

sim_by_row <- function(df, clim = 'notrain') {
  result <- list()  # Initialize a list to store results
  for (i in 1:nrow(df)) {
    s <- simulate_seasonal_climate(
      xmin = df$Min_Month_Value[i],
      xmax = df$Max_Month_Value[i],
      xvar = 0,
      seasons = 1,
      years = 1
    )
    if(clim == 'rain'){
      s[s<0] <- 0
    }
    result[[i]] <- s  # Store the result for this row in the list
  }
  return(result)  # Return the list of results
}


addnames <- function(l, newname){
  names(l) <- paste(newname, seq(1:length(l)), sep = '_')
  return(l)
}

gen_clim_list <- function(df, clim = 'notrain', newname){
    x <- sim_by_row(df)
    x <- addnames(x, newname)
    return(x)
}

# generate climate time series
temperate_list <- gen_clim_list(df = t17, newname = 'normal_temperate')
warm_list <- gen_clim_list(df = t23, newname = 'normal_warm')
hot_list <- gen_clim_list(df = t29, newname = 'normal_hot')

dry_list <- gen_clim_list(df = dry, clim = 'rain', newname = 'normal_dry')
moderate_list <- gen_clim_list(df = mod, clim = 'rain', newname = 'normal_moderate')
wet_list <- gen_clim_list(df = wet, clim = 'rain', newname = 'normal_wet')

# save climate time series
savedir <- '../data/sim_climate/'
saveRDS(temperate_list, paste0(savedir, 'normal_temperate_clim.RData'))
saveRDS(warm_list, paste0(savedir, 'normal_warm_clim.RData'))
saveRDS(hot_list, paste0(savedir, 'normal_hot_clim.RData'))
saveRDS(dry_list, paste0(savedir, 'normal_dry_clim.RData'))
saveRDS(moderate_list, paste0(savedir, 'normal_moderate_clim.RData'))
saveRDS(wet_list, paste0(savedir, 'normal_wet_clim.RData'))

