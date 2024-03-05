# function to simulate a climate time series
simulate_seasonal_climate <- function(xmin, xmax, xvar, seasons, years){
  t <- seq(from = 1, to = 365 * years, by = 1)
  error <- rnorm(t, mean = 0, sd = xvar)
  frequency <- seasons * 2 * years * pi / length(t)
  ts <- (xmax + xmin)/2 + (xmax - xmin)/2 * sin(t * frequency) + error
  return(ts)
}

# function to generate an extreme event for a given climate time series
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
  return(list(x, start))  
}

# create a list of normal and extreme events
ee_list <- function(x, s, years, iter, magnitudes, durations, timing, time1, time2){
  # generate 'normal' time series
  x1 <- lapply(1:iter, function(i) 
    simulate_seasonal_climate(
      xmin = x$min
      , xmax = x$max
      , xvar = x$var
      , seasons = s
      , years = years)
  )
  names(x1) <- paste0('iter', seq(1,iter), '_normal')
  # generate 'extreme' events for each 'normal' time series
  newlist <- x1
  for(mag in magnitudes){
    for(dur in durations){
      for(tim in timing){
        x2 <- lapply(x1, function(i) 
          generate_extreme_event(x = i, magnitude_change = mag, time1 = time1, time2 = time2, duration = dur, timing = tim))
        names(x2) <- paste0('iter', seq(1, iter), '_', mag, 'I_', dur, 'D_', tim)
        newlist <- c(newlist, x2)
      }
    }
  }
  return(newlist)
}
