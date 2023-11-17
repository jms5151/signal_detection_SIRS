# climate variables
CLIM_mean = 20.4
CLIM_amp_small = 4.5
CLIM_amp_large = 15
CLIM_trend = 0.08 #average annual change globally since 1980

# generate climate with small amplitude
clim_small <- generate_climate(
  climate_mean = CLIM_mean
  , amplitude = CLIM_amp_small
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
# plot.ts(clim_small_trend, col = 'maroon')
lines(clim_small)

# generate climate with large amplitude
clim_large <- generate_climate(
  climate_mean = CLIM_mean
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
