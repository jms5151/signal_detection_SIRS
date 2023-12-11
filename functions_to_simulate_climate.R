generate_climate <- function(climate_mean, amplitude, daily_var, t){
  error <- rnorm(t, mean = 0, sd = daily_var)
  # classic calculation
  # vertical_shift + amplitude * sin(period * pi + t/phase_shift) #+ error
  # following Huber et al, where amplitude = Tmax - Tmin
  climate_mean + amplitude * sin((2 * pi) / 365 * t) + error
}

generate_climate_with_trend <- function(climate_mean, amplitude, yearly_trend , t){
  # error <- rnorm(t, mean = 0, sd = 0.5)
  climate_mean + amplitude * sin((2 * pi) / 365 * t) + (yearly_trend  * t / 365) #+error
}

generate_extreme_event <- function(x, magnitude_change, duration, timing){
  # n works if there's no error, if there's error may need to do this smarter (i.e., within the first wave)
  EV_timing <- which(x == max(x[1:365]))[1] # first peak in year 1 
  if(timing == 'pre'){
    EV_timing <- EV_timing - 30
  } else if(timing == 'post'){
    EV_timing <- EV_timing + 30
  }
  start <- EV_timing - round((duration-1)/2)
  end <- EV_timing + round((duration-1)/2)
  x[start:end] <- x[start:end] + magnitude_change
  return(x)  
}
