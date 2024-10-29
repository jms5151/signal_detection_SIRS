### simulate extreme events
source('functions_to_simulate_climate.R')

t.Magnitudes = seq(1, 15, 1)
t.Durations = seq(1, 20, 1)
r.Magnitudes = seq(10, 100, 10)
r.Durations = seq(1, 7, 1) 

# list files
climdir <- '../data/sim_climate/'
cfilepaths <- list.files(climdir, full.names = T)

susceptdir <- '../data/sim_sids/'
sfilepaths <- list.files(susceptdir, full.names = T)

# low and high susceptibility
stypes <- c('S_min_t', 'S_max_t')

# function to generate lists of extreme event climate time series
generate_ee_lists <- function(climpaths, susppaths, magnitude_change, duration, stypes){
  for(i in 1:length(climpaths)){
    c <- readRDS(climpaths[i])
    s <- readRDS(susppaths[i])
    for(j in stypes){
      ee_time_series <- list()
      ee_start_times <- list()
      slist <- unlist(lapply(s, '[', j))
      for(k in magnitude_change){
        for(l in duration){
          # Apply generate_extreme_event to all lists in c and smins using Map
          ee <- Map(generate_extreme_event, x = c, time = slist, magnitude_change = k, duration = l)
          ee_ts <- lapply(ee, function(x) x[[1]])
          names(ee) <- gsub('normal_', '', names(ee))
          names(ee_ts) <- paste0(names(ee), '_', k, 'I_', l, 'D')
          ee_time_series <- c(ee_time_series, ee_ts)
          # separate start times and save
          ee_start <- lapply(ee, function(x) x[[2]])
          names(ee_start) <- gsub('normal_', '', names(ee_start))
          names(ee_start) <- paste0(names(ee), '_', k, 'I_', l, 'D')
          ee_start_times <- c(ee_start_times, ee_start)
        }
      }
      eeFileName1 <- gsub('sim_climate/normal_', paste0('sim_climate_ee/', j, '_'), climpaths[i])
      saveRDS(ee_time_series, file = eeFileName1)
      eeFileName2 <- gsub('climate_ee/', 'start_times/', eeFileName1)
      eeFileName2 <- gsub('_clim', '', eeFileName2)
      saveRDS(ee_start_times, file = eeFileName2)
      }
  }
}

# separate rainfall and temperature
rainregimes <- 'dry|moderate|wet'
tempregimes <- 'temperate|warm|hot'

rainpaths <- cfilepaths[grepl(rainregimes, cfilepaths)]
rspaths <- sfilepaths[grepl(rainregimes, sfilepaths)]

temppaths <- cfilepaths[grepl(tempregimes, cfilepaths)]
tspaths <- sfilepaths[grepl(tempregimes, sfilepaths)]

# rain
generate_ee_lists(climpaths = rainpaths, susppaths = rspaths, magnitude_change = r.Magnitudes, duration = r.Durations, stypes = stypes)

# temperature
generate_ee_lists(climpaths = temppaths, susppaths = tspaths, magnitude_change = t.Magnitudes, duration = t.Durations, stypes = stypes)
