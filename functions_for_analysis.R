# Function to get segment
get_segment <- function(data, start_time) {
  end_time <- start_time + 180
  subset(data, time >= start_time & time <= end_time)
}

# Re calculations
vbd_calc_Re <- function(x, gamma){
  x$Re = x$beta * x$S / gamma
  return(x)
}

wbd_calc_Re <- function(x, beta1, mu, gamma){
  x$Re = (beta1 + x$beta) * x$S / (mu + gamma)
  return(x)
}

# Function to compute summary statistics
compute_stats <- function(df) {
  summary_stats <- data.frame(
    # Final size
    cumulative_cases = sum(df$I)
    # Susceptibility (serology)
    , max_S = max(df$S)
    # Peak timing
    , peak_timing_cases = which(df$I == max(df$I))    
    # Max cases
    , max_cases = max(df$I)
    # Peakiness (kurtosis)
    , Peakiness = moments::kurtosis(df$I)
  )
  summary_stats
}

process_sir_output <- function(datalist, start_list, model_type, params, explabel){
  # calculate Re values
  if (model_type == 'vbd') {
    df <- lapply(datalist, function(x) vbd_calc_Re(x, gamma = vbd.gamma))
  } else {
    df <- lapply(datalist, function(x) wbd_calc_Re(x, beta1 = wbd.beta1, mu = mu, gamma = wbd.gamma))
  }
  
  dfshort <- Map(get_segment, data = df, start_time = start_list)
  results <- Map(compute_stats, df = dfshort)
  
  # flatten
  results_flattened <- bind_rows(lapply(results, bind_rows, .id = "Dataset"), .id = "List")
  
  # add label
  results_flattened$experiment <- explabel
  
  return(results_flattened)
}
