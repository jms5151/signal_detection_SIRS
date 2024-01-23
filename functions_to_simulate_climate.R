simulate_daily_temperature <- function(climate_mean, amplitude, daily_var, t){
  error <- rnorm(t, mean = 0, sd = daily_var)
  # classic calculation
  # vertical_shift + amplitude * sin(period * pi + t/phase_shift) #+ error
  # following Huber et al, where amplitude = Tmax - Tmin
  climate_mean + amplitude * sin((2 * pi) / 365 * t) + error
}

temperature_with_trend <- function(climate_mean, amplitude, yearly_trend, t, variability_coef) {
  error <- rnorm(t, mean = 0, sd = variability_coef * t / 365)
  climate_mean + amplitude * sin((2 * pi) / 365 * t) + (yearly_trend * t / 365) + error
}

generate_extreme_event <- function(x, time1, time2, magnitude_change, duration, timing){
  EE_timing <- which(x == max(x[time1:time2]))[1] # peak between time 1 and time 2 
  if(timing == 'pre'){
    EE_timing <- EE_timing - 30
  } else if(timing == 'post'){
    EE_timing <- EE_timing + 30
  }
  if((duration %% 2) == 0){ # if even
    start <- EE_timing - ((duration/2) - 1)
    end <- EE_timing + duration/2
  } else {
    start <- EE_timing - (duration-1)/2
    end <- EE_timing + (duration-1)/2
  }
  x[start:end] <- x[start:end] + magnitude_change
  return(x)  
}

simulate_daily_rainfall <- function(t, avg_days_with_rain_per_month, mean_total_rainfall_per_month) {
  # Initialize daily rainfall vector
  rainfall <- numeric(length(t))
  
  # Validate input vectors
  if (length(avg_days_with_rain_per_month) != 12 || length(mean_total_rainfall_per_month) != 12) {
    stop("Input vectors must have 12 values, one for each month.")
  }
  
  # Loop through each day in the simulation
  for (day in 1:length(t)) {
    # Determine the current month
    current_month <- ((day - 1) %% 360) %/% 30 + 1
    
    # Ensure non-negative values for avg_days_with_rain_per_month
    avg_days_with_rain <- max(0, avg_days_with_rain_per_month[current_month])
    
    # Generate random number of rainy days for the month using rpois
    rainy_days <- max(0, rpois(1, lambda = avg_days_with_rain))
    
    # Check if it's a rainy day
    if (is.finite(rainy_days) && rainy_days > 0) {
      # Ensure non-negative values for mean_total_rainfall_per_month
      mean_rainfall <- max(0, mean_total_rainfall_per_month[current_month] / rainy_days)
      
      # Generate random daily rainfall for each rainy day
      if (runif(1) < 1 / rainy_days) {
        # rainfall[day] <- rnorm(1, mean = mean_rainfall, sd = mean_rainfall * 0.10)
        rainfall[day] <- rexp(1, rate = 1 / mean_rainfall)
      }
    }
  }
  
  return(rainfall)
}

ee_list <- function(climType, times, iter, var1, var2, var3, magnitudes, durations, timing, time1, time2){
  if(climType == 'temperature'){
    x1 <- lapply(1:iter, function(i) simulate_daily_temperature(climate_mean = var1, amplitude = var2, daily_var = var3, t = times))
    magUnit <- 'C'
  } else if(climType == 'rainfall'){
    x1 <- lapply(1:iter, function(i) simulate_daily_rainfall(t = times, avg_days_with_rain_per_month = var1, mean_total_rainfall_per_month = var2))
    magUnit <- 'mm'
  }
  names(x1) <- paste0('iter', seq(1,iter), '_normal')
  newlist <- x1
  for(mag in magnitudes){
    for(dur in durations){
      for(tim in timing){
        x2 <- lapply(x1, function(i) 
          generate_extreme_event(x = i, magnitude_change = mag, time1 = time1, time2 = time2, duration = dur, timing = tim))
        names(x2) <- paste0('iter', seq(1, iter), '_', mag, magUnit, '_', dur, 'd_', tim)
        newlist <- c(newlist, x2)
      }
    }
  }
  return(newlist)
}

