# extreme events simulations

source('functions_to_simulate_climate.R')
# may want the city name in the title too if we're doing multiple locations
# T = temperature
# sh = short term simulation
# lg = long run simulation

gen_T_sh_list <- function(years, iter, cityMean, cityAmp, cityNoise, magnitudes, durations, timing){
  times = seq(1, 365*years, 1)
  x1 <- lapply(1:iter, function(i) generate_climate(cityMean, cityAmp, cityNoise, t = times))
  names(x1) <- paste0('iter', seq(1,iter), '_normal')
  newlist <- x1
  for(mag in magnitudes){
    for(dur in durations){
      for(tim in timing){
        x2 <- lapply(x1, function(i) 
          generate_extreme_event(x = i, magnitude_change = mag, duration = dur, timing = tim))
        names(x2) <- paste0('iter', seq(1, iter), '_', mag, 'C_', dur, 'd_', tim)
        newlist <- c(newlist, x2)
      }
    }
  }
  return(newlist)
}

# Conditions
Magnitudes = seq(5, 25, 5)
Durations = seq(3, 15, 2)
ExEV_times = c('pre', 'peak', 'post')
Yrs = 1
Iterations = 50

TS_BRAZIL <- gen_T_sh_list(
  years = Yrs
  , iter = Iterations
  , cityMean = 25
  , cityAmp = 5
  , cityNoise = 1.5
  , magnitudes = Magnitudes
  , durations = Durations
  , timing = ExEV_times
)


# climate change simulations


# climate variables
CLIM_mean = 20.4 # Sao Paulo, Brazil
CLIM_amp_small = 3.2 # Sao Paulo, Brazil
CLIM_amp_large = 15
CLIM_trend = 0.08 #average annual change globally since 1980
dv = 1.5
CLIM_mean_Dhaka <- 26.1
CLIM_amp_Dhaka <- 7 # uneven, 7C below mean to 2.9C above mean

clim_dhaka <- generate_climate(
  climate_mean = CLIM_mean_Dhaka
  , amplitude = CLIM_amp_Dhaka
  , daily_var = dv
  , t = times
)

# generate climate with small amplitude
clim_small <- generate_climate(
  climate_mean = CLIM_mean
  , amplitude = CLIM_amp_small
  , daily_var = dv
  , t = times
)

# generate climate with small amplitude & increasing trend
clim_small_trend <- generate_climate_with_trend(
  climate_mean = CLIM_mean
  , amplitude = CLIM_amp_small
  , t = times
  , yearly_trend =  CLIM_trend 
)

# # plot
plot.ts(clim_small_trend, col = 'maroon')
# lines(clim_small)

# generate climate with large amplitude
clim_large <- generate_climate(
  climate_mean = CLIM_mean
  , daily_var = dv
  , amplitude = CLIM_amp_large
  , t = times
)

# generate climate with large amplitude & increasing trend
clim_large_trend <- generate_climate_with_trend(
  climate_mean = CLIM_mean
  , amplitude = CLIM_amp_large
  , t = times
  , yearly_trend =  CLIM_trend
)

# # plot
# plot.ts(clim_large_trend, col = 'maroon')
# lines(clim_large)



# extreme_climate <- extreme_event_generation(x = normal_climate, n = 1, magnitude_change = 4, duration = 3)
# plot.ts(normal_climate, ylim = c(15, 35))
# lines(extreme_climate, col = 'orange')
# lines(normal_climate, col = 'lightgreen')
