# Re calculations
vbd_calc_Re <- function(x, gamma){
  x$Re = x$beta * x$S / gamma
  return(x)
}

wbd_calc_Re <- function(x, beta1, mu, gamma){
  x$Re = (beta1 + x$beta) * x$S / (mu + gamma)
  return(x)
}

# Return time function
calc_return_time <- function(x, y){
  count = 0
  for (i in 1:length(x)) {
    if (abs(x[i] - y[i]) / x[i] <= 0.001) {
      count = count + 1
      if (count >= 7) {
        return(i - 6) # Return the starting point of synchronization
      }
    } else {
      count = 0
    }
  }
  
  return(99999) # Return NA if no synchronization is found within the criteria
}

create_control_list <- function(x){
  c.list <- x[grepl('normal', names(x))]
  names(c.list) <- gsub('_normal', '', names(c.list))
  return(c.list)
}

add_start_time <- function(experiment_data, start_time) {
  experiment_data$Start_Time <- start_time
  experiment_data
}

combine_experiment_with_start <- function(experiment_data, start_time) {
  list(data = experiment_data, Start_Time = start_time)
}

create_experiments_list <- function(x, start_times){
  experiments <- x[!grepl('normal', names(x))]
  control_groups <- unique(gsub('_.*', '', names(x)))
  e.list <- list()
  for(i in 1:length(control_groups)){
    # list of experimental simulations
    templist <- experiments[grepl(paste0(control_groups[i], '_'), names(experiments))]
    # add start time
    stimes <- start_times[grepl(paste0(control_groups[i], '_'), names(start_times))]
    templist <- Map(combine_experiment_with_start, templist, stimes)
    # remove iteration number from list names
    names(templist) <- gsub(paste0(control_groups[i], '_'), '', names(templist))
    # append list
    e.list[[i]] <- templist
  }
  names(e.list) <- control_groups
  return(e.list)
}

# Function to get segment
get_segment <- function(data, start_time, window_length) {
  if(is.na(window_length) == T){
    end_time <- nrow(data)
  } else {
    end_time <- start_time + window_length - 1
  }
  subset(data, time >= start_time & time <= end_time)
}

# Function to compute summary statistics
compute_stats <- function(experiment_data, control_data) {
  # Replace this with your actual summary statistics calculation
  summary_stats <- data.frame(
    # Experiment_Mean = mean(experiment_data$I),
    # Control_Mean = mean(control_data$I)
    # Final size
    Control_final_size = sum(control_data$I)
    , Experiment_final_size = sum(experiment_data$I)
    # Mean Beta
    , Control_mean_beta = mean(control_data$beta)
    , Experiment_mean_beta = mean(experiment_data$beta)
    # SD Beta
    , Control_beta_sd = sd(control_data$beta)
    , Experiment_beta_sd = sd(experiment_data$beta)
    # Kurtosis
    , Control_Re_kurtosis = moments::kurtosis(control_data$Re)
    , Experiment_Re_kurtosis = moments::kurtosis(experiment_data$Re)
    # Min Re
    , Control_min_Re = min(control_data$Re)
    , Experiment_min_Re = min(experiment_data$Re)
    # Mean Re
    , Control_mean_Re = mean(control_data$Re)
    , Experiment_mean_Re = mean(experiment_data$Re)
    # Max Re
    , Control_max_Re = max(control_data$Re)
    , Experiment_max_Re = max(experiment_data$Re)
    # Peak timing
    , Control_max_Re_timing = which(control_data$Re == max(control_data$Re))
    , Experiment_max_Re_timing = which(experiment_data$Re == max(experiment_data$Re))
    # SD Suscpetible
    , Control_S_sd = sd(control_data$S)
    , Experiment_S_sd = sd(experiment_data$S)
    # AUC Infected
    , Control_auc_I = pracma::trapz(control_data$I)
    , Experiment_auc_I = pracma::trapz(experiment_data$I)
    # auc_Re <- trapz(x$Re)
    # Return time
    , Return_time = calc_return_time(x = control_data$I, y = experiment_data$I)
    # Correlation
    , CE_Correlation = cor(control_data$I, experiment_data$I)
  )
  summary_stats
}

# Function to process each control and its experiments
process_control_group <- function(control_data, experiments, window_length) {
  lapply(experiments, function(experiment) {
    start_time <- experiment$Start_Time
    experiment_data <- experiment$data
    control_segment <- get_segment(control_data, start_time, window_length)
    experiment_segment <- get_segment(experiment_data, start_time, window_length)
    
    compute_stats(experiment_segment, control_segment)
  })
}

# Define a function that performs the calculations for a specific country, model type, and window length
parallel_function <- function(country, model_type, window_length) {
  # open files
  fileNameMain <- paste0(scratch_path, 'results/ee_', model_type, '_', country, '.RData')
  df <- readRDS(fileNameMain)

  # calculate Re values
  if (model_type == 'vbd') {
    df <- lapply(df, function(x) vbd_calc_Re(x, gamma = vbd.gamma))
  } else {
    df <- lapply(df, function(x) wbd_calc_Re(x, beta1 = wbd.beta1, mu = mu, gamma = wbd.gamma))
  }
  
  # separate into control and experiment lists
  data_list <- create_control_list(x = df)
  fileNameStarts <- paste0(scratch_path, 'data/ee_start_time_', country, '.RData')
  startTimes <- readRDS(fileNameStarts)
  experiment_list <- create_experiments_list(x = df, start_times = startTimes)
  
  # compute stats
  results <- lapply(names(data_list), function(control_name) {
    control_data <- data_list[[control_name]]
    experiments <- experiment_list[[control_name]]
    process_control_group(control_data, experiments, window_length)
  })
  
  # flatten
  results_flattened <- bind_rows(lapply(results, bind_rows, .id = "Dataset"), .id = "List")
  
  # save
  if(is.na(window_length) == T){
    windowname = 'long'
  } else {
    'short'
  }
  saveName <- paste0(paste0(scratch_path, 'results/ee_sumarized_'), paste(model_type, country, windowname, sep='_'), '.RData')
  saveRDS(results_flattened, file = saveName)
}