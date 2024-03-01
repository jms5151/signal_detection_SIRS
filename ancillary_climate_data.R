# import temperature and rainfall data
library(tidyverse)

## Functions -----------------------------------
calc_daily_variability <- function(df){
  df$dm <- format(df$time, '%m-%d')
  df <- df %>%
    group_by(dm) %>%
    summarise(daily_var = sd(X0))
  mean(df$daily_var)
}

# historical_amp <- function(df){
#   df$year <- format(df$time, '%Y')
#   df <- df %>%
#     group_by(year) %>%
#     summarise(amp = (max(X0) - min(X0))/2)
#   mean(df$amp)
# }

# should anomalies be deviation from mean or threshold?
# should intensity be a weighted average instead of mean or max?
detect_anomalies <- function(values, mean_value, threshold) {
  
  # Identify values exceeding the threshold
  exceedances <- values > threshold
  
  # Create a vector to identify consecutive exceedances
  consecutive_exceedances <- rle(exceedances)
  
  # Initialize data frame to store anomalies, intensity, duration, consecutive values, and anomaly values
  anomalies_df <- data.frame(Start = integer(), End = integer(), Max_Value = numeric(), Intensity = numeric(), Duration = integer(), Consecutive_Values = integer(), Anomaly_Values = character())
  
  # Loop through anomalies and calculate intensity, duration, consecutive values, and anomaly values within each anomaly
  for (i in seq_along(consecutive_exceedances$values)) {
    if (consecutive_exceedances$values[i]) {
      start_index <- sum(consecutive_exceedances$lengths[1:(i-1)]) + 1
      end_index <- start_index + consecutive_exceedances$lengths[i] - 1
      anomaly_values <- values[start_index:end_index] - mean_value  # Calculate anomaly as the difference from the mean
      
      max_value <- max(anomaly_values)
      intensity <- round(mean(anomaly_values))
      duration <- length(anomaly_values)
      
      anomalies_df <- rbind(anomalies_df, data.frame(Start = start_index, End = end_index, Max_Value = max_value, Intensity = intensity, Duration = duration))
    }
  }
  
  return(anomalies_df)
}


prob_ee <- function(df, timespan){
  df <- df %>%
    group_by(Duration, Intensity) %>%
    summarise(ee_count = length(Intensity),
              prob = ee_count / timespan)
  return(df)
}

temp_cc_trends <- function(df){
  df$year <- format(df$time, '%Y')
  df <- df %>%
    group_by(year) %>%
    summarise(mean = mean(X0),
              amp = (max(X0) - min(X0))/2)
  clim_regress <- lm(df$mean ~ 0 + as.numeric(df$year))
  slope <- unname(clim_regress$coefficients)
  amp_change <- sd(df$amp[nrow(df)-10:nrow(df)]) - sd(df$amp[1:10])
  return(data.frame('cc_slope' = slope, 'cc_amp_change' = amp_change))
}

calculate_precip_metrics <- function(df){
  df$year <- format(df$time, format = '%Y')
  df$month <- format(df$time, '%m')
  df <- df %>% group_by(year, month) %>% summarise(total_rainfall = sum(X0),
                                                   rainy_days = sum(X0 > 1),
                                                   max_rain = max(X0))
  return(df)
}

avg_precip_metrics <- function(df){
  df <- df %>% group_by(month) %>% summarise(total_rainfall = mean(total_rainfall), rainy_days = mean(rainy_days))
  return(df)
}

precip_cc_trends <- function(df){
  df <- df %>%
    group_by(year) %>%
    summarise(mean = mean(total_rainfall),
              max = max(max_rain, na.rm = T),
              total_rainfall = sum(total_rainfall),
              rainy_days = sum(rainy_days)
    )
  return(df)
}


create_T_metrics <- function(df, t){
  # format
  df$time <- as.Date(as.character(df$time))
  T_hist <- subset(df, time <= t)
  T_fut <- subset(df, time > t)
  
  # calculate historical metrics for simulations
  hist_var <- calc_daily_variability(df = T_hist)
  hist_mean <- mean(T_hist$X0)
  hist_amp <- historical_amp(df = T_hist)
  
  # calculate historical extreme events
  yrs_hist <- round(as.numeric(difftime(T_hist$time[nrow(T_hist)], T_hist$time[1], units = 'weeks'))/52)
  thr <- quantile(T_hist$X0, 0.90)
  anomalies_result_hist <- detect_anomalies(values = T_hist$X0, threshold = thr, mean_value = hist_mean)
  eeprobs_hist <- prob_ee(df = anomalies_result_hist, timespan = yrs_hist)
  
  # calculate probability of future extreme events
  yrs_fut <- round(as.numeric(difftime(T_fut$time[nrow(T_fut)], T_fut$time[1], units = 'weeks'))/52)
  anomalies_result_fut <- detect_anomalies(values = T_fut$X0, threshold = thr, mean_value = hist_mean)
  eeprobs_fut <- prob_ee(df = anomalies_result_fut, timespan = yrs_fut)
  
  # calculate climate change metrics for simulations
  cc_temp <- temp_cc_trends(df = T_fut)
  
  return(list('hist_var' = hist_var
             , 'hist_mean' = hist_mean
             , 'hist_amp' = hist_amp
             , 'thr' = thr
             , 'prob_ee_hist' = eeprobs_hist
             , 'prob_ee_fut' = eeprobs_fut
             , 'cc_metrics' = cc_temp))
}

create_P_metrics <- function(df, t){
  # format
  df$time <- as.Date(as.character(df$time))
  P_hist <- subset(df, time <= t)
  P_fut <- subset(df, time > t)
  
  # calculate historical metrics for simulations
  hist_precip <- calculate_precip_metrics(P_hist)
  hist_precip_metrics <- avg_precip_metrics(hist_precip)

  # calculate historical extreme events
  yrs_hist <- round(as.numeric(difftime(P_hist$time[nrow(P_hist)], P_hist$time[1], units = 'weeks'))/52)
  thr <- quantile(P_hist$X0, 0.90)
  hist_mean <- mean(P_hist$X0)
  anomalies_result_hist <- detect_anomalies(values = P_hist$X0, threshold = thr, mean_value = hist_mean)
  eeprobs_hist <- prob_ee(df = anomalies_result_hist, timespan = yrs_hist)
  
  # calculate probability of future extreme events
  yrs_fut <- round(as.numeric(difftime(P_fut$time[nrow(P_fut)], P_fut$time[1], units = 'weeks'))/52)
  anomalies_result_fut <- detect_anomalies(values = P_fut$X0, threshold = thr, mean_value = hist_mean)
  eeprobs_fut <- prob_ee(df = anomalies_result_fut, timespan = yrs_fut)
  
  # calculate climate change metrics for simulations
  fut_precip <- calculate_precip_metrics(P_fut)
  fut_precip_metrics <- avg_precip_metrics(fut_precip)
  cc_precip <- precip_cc_trends(df = fut_precip)
  
  return(list('hist_precip' = hist_precip
              , 'hist_precip_metrics' = hist_precip_metrics
              , 'hist_mean' = hist_mean
              , 'thr' = thr
              , 'prob_ee_hist' = eeprobs_hist
              , 'prob_ee_fut' = eeprobs_fut
              , 'fut_precip' = fut_precip
              , 'fut_precip_metrics' = fut_precip_metrics
              , 'cc_metrics' = cc_precip))
}

# plot
plot_future_rain <- function(dfList, locName){
  figDir <- '../figures/'
  
  df1 <- dfList[[7]]
  p1 <- ggplot(df1, aes(year, rainy_days)) + geom_point() + facet_wrap(~month) + theme_bw() + ylab('Rainy Days (#)') + scale_x_discrete(breaks=seq(2020, 2080, 20))
  ggsave(p1, file = paste0(figDir, locName, '_rainy_days.pdf'))
  p2 <- ggplot(df1, aes(year, total_rainfall)) + geom_point() + facet_wrap(~month) + theme_bw() + ylab('Total Rainfall (mm)') + scale_x_discrete(breaks=seq(2020, 2080, 20))
  ggsave(p2, file = paste0(figDir, locName, '_total_rain.pdf'))
  p3 <- ggplot(df1, aes(year, max_rain)) + geom_point() + facet_wrap(~month) + theme_bw() + ylab('Maximum Rainfall (mm)') + scale_x_discrete(breaks=seq(2020, 2080, 20))
  ggsave(p3, file = paste0(figDir, locName, '_max_rain.pdf'))
  df2 <- dfList[[9]]
  
  pdf(paste0(figDir, locName, '_annual_future_rainfall.pdf'))
  par(mfrow = c(2,2))
  plot(df2$year, df2$rainy_days, pch = 16, ylab = 'Rainy Days (#)', xlab = 'Year')
  plot(df2$year, df2$total_rainfall, pch = 16, ylab = 'Total Rainfall (mm)', xlab = 'Year')
  plot(df2$year, df2$max, pch = 16, ylab = 'Maximum Rainfall (mm)', xlab = 'Year')
  plot(df2$year, df2$mean, pch = 16, ylab = 'Mean Rainfall (mm)', xlab = 'Year')
  dev.off()
}

## Run --------------------------------------------
## Needed to save the data with different file names and format the date
historical_period_end <- '2014-12-31'

climate_data <- list.files('../data/climate/', full.names = T)
countries <- c('Sudan', 'Ethiopia', 'India', 'China', 'Haiti', 'Italy', 'Pakistan', 'Philippines', 'Fiji', 'Brazil', 'BurkinaFaso')

for(i in climate_data){
  x <- read.csv(i)  
  countryName <- countries[sapply(countries, grepl, i)]
  if(grepl('precip', i) == TRUE){
    x1 <- create_P_metrics(df = x, t = historical_period_end)
    saveRDS(x1, file = paste0('../data/precip_metrics_', countryName, '.RData'))
    plot_future_rain(dfList = x1, locName = countryName)
  } 
  else {
    x1 <- create_T_metrics(df = x, t = historical_period_end)
    saveRDS(x1, file = paste0('../data/temp_metrics_', countryName, '.RData'))
  }
}

