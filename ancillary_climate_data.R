# import temperature and rainfall data
library(tidyverse)

## Functions -----------------------------------
format_dates <- function(df){
  # df <- na.omit(df)
  df$time <- as.Date(as.character(df$time), '%Y-%m-%d')
  df$year <- format(df$time, '%Y')
  df$month <- format(df$time, '%m')
  df$month_day <- format(df$time, '%m-%d')
  return(df)
}

calc_daily_variability <- function(df){
  df <- df %>%
    group_by(month_day) %>%
    summarise(daily_var = sd(X0))
  return(mean(df$daily_var))
}

mmm <- function(df){
  df <- df %>%
    group_by(month) %>%
    summarise(monthmean = mean(X0)) 
  return(max(df$monthmean))
} 

base_clim <- function(df){
  # calculate min, mean, median, 90th, max
  q <- quantile(T_hist$X0, probs = c(0,0.5,0.9,1))
  dfnew <- as.data.frame(t(q))
  colnames(dfnew) <- c('min', 'median', 'q90', 'max')
  dfnew$mean <- mean(T_hist$X0)
  dfnew$mmm <- mmm(T_hist)
  dfnew$sd <- sd(T_hist$X0)
  # calculate variability
  dfnew$var <- calc_daily_variability(df = T_hist)
  # return data frame
  return(dfnew)
}

# should anomalies be from long term mean (as currently done)
# or above summertime mean?
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


## run -------------------------------------------------
historical_period_end <- as.Date('2014-12-31', '%Y-%m-%d')

climate_data <- list.files('../data/climate/', full.names = T)
countries <- c('Sudan', 'Ethiopia', 'India', 'China', 'Haiti', 'Italy', 'Pakistan', 'Philippines', 'Fiji', 'Brazil', 'BurkinaFaso')

for(i in climate_data){
  # read file
  x <- read.csv(i)  
  # format
  x <- format_dates(x)
  # subset
  T_hist <- subset(x, time <= historical_period_end)
  T_fut <- subset(x, time > historical_period_end)
  # base characteristics
  base_metrics <- base_clim(df = T_hist)
  # calculate historical extreme events
  yrs_hist <- round(as.numeric(difftime(T_hist$time[nrow(T_hist)], T_hist$time[1], units = 'weeks'))/52)
  anomalies_result_hist <- detect_anomalies(values = T_hist$X0, threshold = base_metrics$q90, mean_value = base_metrics$mmm)
  eeprobs_hist <- prob_ee(df = anomalies_result_hist, timespan = yrs_hist)
  # calculate probability of future extreme events
  yrs_fut <- round(as.numeric(difftime(T_fut$time[nrow(T_fut)], T_fut$time[1], units = 'weeks'))/52)
  anomalies_result_fut <- detect_anomalies(values = T_fut$X0, threshold = base_metrics$q90, mean_value = base_metrics$mmm)
  eeprobs_fut <- prob_ee(df = anomalies_result_fut, timespan = yrs_fut)
  # combine output  
  x1 <- list('base_metrics' = base_metrics
              , 'ee_hist' = eeprobs_hist
              , 'ee_fut' = eeprobs_fut)
  # save
  countryName <- countries[sapply(countries, grepl, i)]
  saveRDS(x1, file = paste0('../data/climate_metrics_', countryName, '.RData'))
}


### wikipedia data
library(tidyverse)

wprecip <- read.csv('../data/wikipedia_precip.csv')
wtemp <- read.csv('../data/wikipedia_temp.csv')
wtemp <-wtemp[wtemp$Country != "",]

# Step 1: Calculate Row-wise Min and Max
add_monthly_values <- function(df){
  df <- df %>%
    rowwise() %>%
    mutate(
      Min_Month_Value = min(c_across(Jan:Dec), na.rm = TRUE),
      Max_Month_Value = max(c_across(Jan:Dec), na.rm = TRUE),
      Range = Max_Month_Value - Min_Month_Value
    ) %>%
    ungroup()
  return(df)
}

# Step 2: Calculate 90th percentile values
source('functions_to_simulate_climate.R')
addp90 <- function(df, clim = 'notrain'){
  df$p90 <- NA
  for(i in 1:nrow(df)){
    s <- simulate_seasonal_climate(
      xmin = df$Min_Month_Value[i]
      , xmax = df$Max_Month_Value[i]
      , xvar = 0
      , seasons = 1
      , years = 1 )
    if(clim == 'rain'){
      s[s<0] <- 0
    }
    df$p90[i] <- round(quantile(s[s>0], 0.90))
  }
  return(df)  
}

wtemp <- add_monthly_values(df = wtemp)
wtemp <- addp90(df = wtemp)

wprecip <- add_monthly_values(df = wprecip)
wprecip <- addp90(df = wprecip)

plotSins <- function(df, colorName){
  for(i in 1:nrow(df)){
    s <- simulate_seasonal_climate(
      xmin = df$Min_Month_Value[i]
      , xmax = df$Max_Month_Value[i]
      , xvar = 0
      , seasons = 1
      , years = 3 )
    lines(s, col = colorName)
  }
}

pdf('../figures/functional_forms/temperature.pdf', width = 8, height = 4)
plot(0, type = 'l', ylim = c(-25,30), xlim = c(1,1000), xlab = 'Time (days)', ylab = 'Temperature (C)')
t29 <- subset(wtemp, p90 == 29)
plotSins(df = t29, 'darkred')
t23 <- subset(wtemp, p90 == 23)
plotSins(df = t23, 'red')
t17 <- subset(wtemp, p90 == 17)
plotSins(df = t17, 'orange')
dev.off()

pdf('../figures/functional_forms/rainfall.pdf', width = 8, height = 4)
plot(0, type = 'l', ylim = c(0,400), xlim = c(1,1000), xlab = 'Time (days)', ylab = 'Rainfall (mm)')
dry <- subset(wprecip, p90 > 75 & p90 < 85)
plotSins(df = dry, 'brown')
mod <- subset(wprecip, p90 > 140 & p90 < 150)
plotSins(df = mod, 'lightblue')
wet <- subset(wprecip, p90 > 340 & p90 < 350)
plotSins(df = wet, 'blue')
dev.off()
