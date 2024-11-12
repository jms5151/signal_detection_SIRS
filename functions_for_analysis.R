# Function to get segment
get_segment <- function(data) {
  start_time = 300
  end_time = 650
  subset(data, time >= start_time & time <= end_time)
}

# Function to compute summary statistics
compute_stats <- function(df, ht) {
  
  peaks <- data.frame(findpeaks(df$I, minpeakheight = ht, minpeakdistance = 10))
  
  if(nrow(peaks) == 0){
    summary_stats <- data.frame(peak_timing = 0, max_incidence = 0, cumulative_proportion = 0, duration = 0, peakiness = 0)
  } else {
    
    colnames(peaks) <- c('Peak_height', 'Peak_timing', 'Start_outbreak', 'End_outbreak')
    
    if(nrow(peaks) > 1){
      idx = which(peaks$Peak_height == max(peaks$Peak_height))
    } else if(nrow(peaks) == 1){
      idx = 1
    }
    
    summary_stats <- data.frame(
      # Peak timing
      peak_timing = peaks$Peak_timing[idx]    
      # Max incidence
      , max_incidence = peaks$Peak_height[idx]
      # Final size
      , cumulative_proportion = trapz(df$I[peaks$Start_outbreak[idx]:peaks$End_outbreak[idx]])
      # Duration largest outbreak
      , duration = peaks$End_outbreak[idx] - peaks$Start_outbreak[idx]
      # Number outbreaks
      , peakiness = nrow(peaks)
    )
    
  }
  
  return(summary_stats)
}

process_sir_output <- function(datalist, model_type, explabel){
  if (model_type == 'wbd') {
    ht_temp = 0.02
  } else {
    ht_temp = 0.10
  }
  
  dfshort <- Map(get_segment, data = datalist)
  results <- Map(compute_stats, df = dfshort, ht = ht_temp)
  
  # flatten
  results_flattened <- bind_rows(lapply(results, bind_rows, .id = "Dataset"), .id = "List")
  
  # add label
  results_flattened$experiment <- explabel
  
  return(results_flattened)
}
