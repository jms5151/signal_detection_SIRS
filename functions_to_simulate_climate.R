generate_climate <- function(climate_mean, amplitude, t){
  error <- rnorm(t, mean = 0, sd = 0.5)
  # classic calculation
  # vertical_shift + amplitude * sin(period * pi + t/phase_shift) #+ error
  # following Huber et al, where amplitude = Tmax - Tmin
  climate_mean + amplitude * sin((2 * pi) / 365 * t) + error
}

generate_climate_with_trend <- function(climate_mean, amplitude, yearly_trend , t){
  # error <- rnorm(t, mean = 0, sd = 0.5)
  climate_mean + amplitude * sin((2 * pi) / 365 * t) + (yearly_trend  * t / 365) #+error
}

generate_extreme_event <- function(x, n, magnitude_change, duration){
  # n works if there's no error, if there's error may need to do this smarter (i.e., within the first wave)
  peak_timing <- which(x == max(x))[n]
  start <- peak_timing - round((duration-1)/2)
  end <- peak_timing + round((duration-1)/2)
  x[start:end] <- x[start:end] + magnitude_change
  return(x)  
}