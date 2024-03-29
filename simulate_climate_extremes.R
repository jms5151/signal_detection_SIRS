### simulate extreme events
source('functions_to_simulate_climate.R')

t.Magnitudes = c(seq(-10, -1, 1), seq(1, 10, 1)) # -10 to 10 excluding 0
t.Durations = seq(1, 30, 1)
r.Magnitudes_Add = seq(10, 100, 10)
r.Magnitudes_Mult = c(seq(1.1, 2, 0.1), seq(2.5, 3, 0.5))
r.Durations = seq(1, 7, 1) 

# list files
climdir <- '../data/sim_climate/'
cfilepaths <- list.files(climdir, full.names = T)

susceptdir <- '../data/sim_sids/'
sfilepaths <- list.files(susceptdir, full.names = T)

# low and high susceptibility
stypes <- c('S_min_t', 'S_max_t')

# function to generate lists of extreme event climate time series
generate_ee_lists <- function(climpaths, susppaths, magnitude_change, duration, stypes, mathtype = T){
  for(i in 1:length(climpaths)){
    c <- readRDS(climpaths[i])
    s <- readRDS(susppaths[i])
    for(j in stypes){
      ee_time_series <- list()
      ee_start_times <- list()
      slist <- unlist(lapply(s, '[', j))
      for(k in magnitude_change){
        if(k < 0){
          kname <- paste0('neg', abs(k))
        } else {
          kname <- k
        }
        for(l in duration){
          # Apply generate_extreme_event to all lists in c and smins using Map
          ee <- Map(generate_extreme_event, x = c, time = slist, magnitude_change = k, duration = l, addition = mathtype)
          ee_ts <- lapply(ee, function(x) x[[1]])
          names(ee_ts) <- paste0(names(ee), '_', kname, 'I_', l, 'D')
          ee_time_series <- c(ee_time_series, ee_ts)
          # separate start times and save
          ee_start <- lapply(ee, function(x) x[[2]])
          names(ee_start) <- paste0(names(ee), '_', j, 'I_', l, 'D')
          ee_start_times <- c(ee_start_times, ee_start)
        }
      }
      eeFileName1 <- gsub('sim_climate/normal_', paste0('sim_climate_ee/', j, '_'), climpaths[i])
      if(mathtype == F){
        eeFileName1 <- gsub('t_', 't_multi_', eeFileName1)
      }
      saveRDS(ee_time_series, file = eeFileName1)
      eeFileName2 <- gsub('climate_ee/', 'start_times/', eeFileName1)
      saveRDS(ee_start, file = eeFileName2)
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

# rain, addition
generate_ee_lists(climpaths = rainpaths, susppaths = rspaths, magnitude_change = r.Magnitudes_Add, duration = r.Durations, stypes = stypes, mathtype = T)

# rain, multiplication
generate_ee_lists(climpaths = rainpaths, susppaths = rspaths, magnitude_change = r.Magnitudes_Add, duration = r.Durations, stypes = stypes, mathtype = F)

# temperature, addition
generate_ee_lists(climpaths = temppaths, susppaths = tspaths, magnitude_change = t.Magnitudes, duration = t.Durations, stypes = stypes, mathtype = T)
