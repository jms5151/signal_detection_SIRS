# import temperature and rainfall data
library(openmeteo)
library(tidyverse)

start.date <- '2023-12-27'#'1940-01-01'
end.date <- '2023-12-31'

temp.bg <- weather_history(
  location = 'Dhaka, Bangladesh'
  , start = start.date
  , end = end.date
  , hourly = 'temperature_2m'
)

temp.br <- weather_history(
  location = 'Sao Paulo, Brazil'
  , start = start.date
  , end = end.date
  , hourly = 'temperature_2m'
  )

rain.hi <- weather_history(
  location = 'Port-au-Prince, Haiti'
  , start = start.date
  , end = end.date
  , hourly = 'precipitation'
)

rain.drc <- weather_history(
  location = 'Kinshasa, Democratic Republic of the Congo'
  , start = start.date
  , end = end.date
  , hourly = 'precipitation'
)


# calculate daily 'noise' for temperature
calculate_daily_temp <- function(x){
  x <- na.omit(x)
  x$date <- as.Date(as.character(x$datetime))
  x <- x %>% group_by(date) %>% summarise(mean = mean(hourly_temperature_2m), sd = sd(hourly_temperature_2m))
  x
}

daily.T.var.bg <- calculate_daily_temp(x = temp.bg)
daily.T.var.br <- calculate_daily_temp(x = temp.br)

calculate_temp_metrics <- function(x){
  x$year <- format(x$date, '%Y')
  x <- x %>% group_by(year) %>% summarise(amp = (max(mean) - min(mean))/2, sd = mean(sd), mean = mean(mean))
  x <- x %>% summarise('mean' = mean(mean), 'sd' = mean(sd), 'amp' = mean(amp))
  x
}

temp.vals.bg <- calculate_temp_metrics(x = daily.T.var.bg)
temp.vals.br <- calculate_temp_metrics(x = daily.T.var.br)

historical.temperature.values <- rbind(temp.vals.bg, temp.vals.br)
historical.temperature.values$loc <- c('bg', 'br')
save(historical.temperature.values, file = 'data/historical_temperature_values.RData')

# calculate monthly number of days and amount of rainfall and compare simulations with real data 
rain_ts <- function(x){
  x <- na.omit(x)
  x$date <- as.Date(as.character(x$datetime))
  x <- x %>% group_by(date) %>% summarise(daily_rainfall = sum(hourly_precipitation))
  return(x)
}

daily.R.drc <- rain_ts(x = rain.drc)
daily.R.hi <- rain_ts(x = rain.hi)

# extreme events
count_consecutive_exceeds <- function(A, X) {
  if (length(A) == 0) {
    stop("Input vector A is empty.")
  }
  
  B <- rep(0, length(A))
  consecutive_count <- 0
  
  for (i in seq_along(A)) {
    if (A[i] > X) {
      consecutive_count <- consecutive_count + 1
      B[i] <- consecutive_count
    } else {
      consecutive_count <- 0
    }
  }
  
  B <- B[B>0]
  return(range(B))
}

ee.vals <- function(x){
  loc90th <- quantile(x, 0.90)
  exceeds <- count_consecutive_exceeds(A = x, X = loc90th)
  data.frame('loc90th' = loc90th, minCons = exceeds[1], maxCons = exceeds[2])
}


ee.clim <- ee.vals(daily.T.var.bg$mean)
ee.clim <- rbind(ee.clim, ee.vals(daily.T.var.br$mean))
ee.clim <- rbind(ee.clim, ee.vals(daily.R.drc$daily_rainfall))
ee.clim <- rbind(ee.clim, ee.vals(daily.R.hi$daily_rainfall))
ee.clim$climate_variable <- c(rep('temperature', 2), rep('rainfall', 2))
ee.clim$loc <- c('bg', 'br', 'drc', 'hi')
ee.clim$max <- c(
    max(daily.T.var.bg$mean)
    , max(daily.T.var.br$mean)
    , max(daily.R.drc$daily_rainfall)
    , max(daily.R.hi$daily_rainfall)
  )

save(ee.clim, file = 'data/historical_extremes.RData')

calculate_precip_metrics <- function(x){
  x <- na.omit(x)
  x$yrmonth <- format(x$date, format = '%Y-%m')
  x$month <- format(x$date, '%m')
  x <- x %>% group_by(yrmonth, month) %>% summarise(total_rainfall = sum(daily_rainfall),
                                             rainy_days = sum(daily_rainfall > 1))
  # needed if there is more than one year of data
  x <- x %>% group_by(month) %>% summarise(total_rainfall = mean(total_rainfall),
                                           rainy_days = mean(rainy_days))
  return(x)
}

monthly.R.var.drc <- calculate_precip_metrics(x = daily.R.drc)
save(monthly.R.var.drc, file = 'data/historical_rainfall_drc.RData')
monthly.R.var.hi <- calculate_precip_metrics(x = daily.R.hi)
save(monthly.R.var.hi, file = 'data/historical_rainfall_hi.RData')

# test
source('functions_to_simulate_climate.R')
simraindrc <- simulate_daily_rainfall(t = seq(1, nrow(daily.R.drc), 1)
                                      , avg_days_with_rain_per_month = R.vals.drc$rainy_days
                                      , mean_total_rainfall_per_month = R.vals.drc$total_rainfall
                                      )
plot.ts(daily.R.drc$daily_rainfall, col = 'blue')
lines(simraindrc)

simrainhi <- simulate_daily_rainfall(t = seq(1, nrow(daily.R.hi), 1)
                                      , avg_days_with_rain_per_month = R.vals.hi$rainy_days
                                      , mean_total_rainfall_per_month = R.vals.hi$total_rainfall
                                     )

plot.ts(daily.R.hi$daily_rainfall, col = 'blue')
lines(simrainhi)

