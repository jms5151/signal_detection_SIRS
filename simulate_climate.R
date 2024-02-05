# extreme events simulations
source('functions_to_simulate_climate.R')
source('time_spans.R')

# Global conditions
t.Magnitudes = seq(1, 15, 1)
t.Durations = seq(1, 30, 1)
r.Magnitudes = seq(10, 300, 10)
r.Durations = seq(1, 14, 1) 
ExEV_times = c('pre', 'peak', 'post')
Iterations = 100

# extreme event occurs in second year of simulation
t1 = 365
t2 = 365 * 2

site_data <- list.files('../data/', full.names = T)
site_data <- site_data[grepl('precip|temp', site_data)]

# short term, extreme events
for(i in site_data){
  x <- readRDS(i)
  # print range of values
  # cat(i, ':\nIntensity = ', max(x[['prob_ee_hist']]$Intensity), ', Duration = ', max(x[['prob_ee_hist']]$Duration), '\n')
  if(grepl('precip', i)){
    v1 = x[['hist_precip_metrics']]$rainy_days
    v2 = x[['hist_precip_metrics']]$total_rainfall
    v3 = NA
    climVar = 'rainfall'
    mag = r.Magnitudes
    dur = r.Durations
    newFileName <- gsub('precip_metrics_', 'ee_data/ee_wbd_', i)
  } else {
    v1 = x[['hist_mean']]
    v2 = x[['hist_amp']]
    v3 = x[['hist_var']]
    climVar = 'temperature'
    mag = t.Magnitudes
    dur = t.Durations
    newFileName <- gsub('temp_metrics_', 'ee_data/ee_vbd_', i)
  }
  ee <- ee_list(
    climType = climVar
    , times = ee.times
    , iter = Iterations
    , var1 = v1
    , var2 = v2
    , var3 = v3
    , magnitudes = mag
    , durations = dur
    , timing = ExEV_times
    , time1 = t1
    , time2 = t2
  )
  
  # save start time of perturbation
  ee_start_times <- lapply(ee, function(x) x[[2]])
  ee_start_times <- ee_start_times[grepl('normal', names(ee_start_times))==F]
  startFileName <- gsub('precip_metrics_|temp_metrics_', 'ee_start_time_', i)
  saveRDS(ee_start_times, file = startFileName)
          
  # save ee time series data
  ee2  <- lapply(ee[!grepl('normal', names(ee))], function(x) x[[1]])
  ee2 <- c(ee[grepl('normal', names(ee))], ee2)
  saveRDS(ee2, file = newFileName)
}

rm(ee)
rm(ee.clim)
rm(ee2)
rm(ee_start_times)
rm(x)