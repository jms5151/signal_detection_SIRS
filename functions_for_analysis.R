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
    summary_stats <- data.frame(peak_timing = 0, cumulative_proportion = 0, outbreak_duration = 0)
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
      # Final size
      , cumulative_proportion = trapz(df$I[peaks$Start_outbreak[idx]:peaks$End_outbreak[idx]])
      # Duration largest outbreak
      , outbreak_duration = peaks$End_outbreak[idx] - peaks$Start_outbreak[idx]
    )
    
  }
  
  return(summary_stats)
}

process_sir_output <- function(datalist, model_type){
  if (model_type == 'wbd') {
    ht_temp = 0.02
  } else {
    ht_temp = 0.10
  }
  
  dfshort <- Map(get_segment, data = datalist)
  results <- Map(compute_stats, df = dfshort, ht = ht_temp)
  
  # flatten
  results_flattened <- bind_rows(lapply(results, bind_rows), .id = "List")
  
  return(results_flattened)
}

# long to wide
spread_by_metric <- function(df, metric){
  ndf2 <- df[c('List', 'regime', 'suscept', metric)] %>%
    spread(key = List, value = metric)
  return(ndf2)
}

# calculate differences by row
calc_diff <- function(df){
  perturbationColumns <- colnames(df)[!grepl(c('List|regime|suscept|control'), colnames(df))]
  df[perturbationColumns] <- df[perturbationColumns]-df$control
  return(df)
}

# go from wide to long again for plotting purposes
gather_by_metric <- function(df, metric){
  dfx <- df %>%
    select(-control) %>%
    gather(key = ee, value = diff, -c('regime', 'suscept'))
  colnames(dfx)[ncol(dfx)] <- paste0(metric, '_diff')
  return(dfx)
}