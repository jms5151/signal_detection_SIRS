# function to simulate a climate time series
simulate_seasonal_climate <- function(xmin, xmax, xvar = 0, seasons = 1, years){
  t <- seq(from = 1, to = 365 * years, by = 1)
  error <- rnorm(t, mean = 0, sd = xvar)
  frequency <- seasons * 2 * years * pi / length(t)
  ts <- (xmax + xmin)/2 + (xmax - xmin)/2 * sin(t * frequency) + error
  return(ts)
}

# function to generate an extreme event for a given climate time series
generate_extreme_event <- function(x, time, magnitude_change, duration){
  if((duration %% 2) == 0){ # if even
    start <- time - ((duration/2) - 1)
    end <- time + duration/2
  } else {
    start <- time - (duration-1)/2
    end <- time + (duration-1)/2
  }
  x[start:end] <- x[start:end] + magnitude_change
  return(list(x))  
}
