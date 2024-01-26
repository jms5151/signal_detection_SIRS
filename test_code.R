x <- read.csv('./data/output_lamb_T15_R11.csv')
quantile(x$beta0)
beta0 <- median(x$beta0)

# msc plots for slideshow
plot.ts(TS_BRAZIL[['iter7_13C_15d_peak']], ylab = 'Temperature', ylim = c(10, 40), col = 'darkred')
lines(TS_BRAZIL[['iter7_5C_5d_peak']], ylab = 'Temperature', ylim = c(10, 40), col = 'purple')
lines(TS_BRAZIL[['iter7_1C_3d_peak']], ylab = 'Temperature', ylim = c(10, 40), col = 'red')
lines(TS_BRAZIL[["iter7_normal"]], ylim = c(10, 40), ylab = 'Temperature')


par(mfrow = c(2, 1), mar = c(4,4, 0.2, 0.2))
plot.ts(TS_BRAZIL[['iter7_13C_15d_peak']], ylab = 'Temperature', ylim = c(10, 40), col = 'darkred', xaxt = 'n', xlab = '')
lines(TS_BRAZIL[['iter7_5C_5d_peak']], ylab = 'Temperature', ylim = c(10, 40), col = 'purple')
lines(TS_BRAZIL[['iter7_1C_3d_peak']], ylab = 'Temperature', ylim = c(10, 40), col = 'red')
lines(TS_BRAZIL[["iter7_normal"]], ylim = c(10, 40), ylab = 'Temperature')

plot.ts(brazil_sir_out[['iter1_13C_15d_peak']]$I, ylab = 'Infected', col = 'darkred')
lines(brazil_sir_out[['iter1_5C_5d_peak']]$I, ylim = c(10, 40), col = 'purple')
lines(brazil_sir_out[['iter1_1C_3d_peak']]$I, col = 'red')
lines(brazil_sir_out[["iter1_normal"]]$I)


plot.ts(brazil_sir_out[["iter1_normal"]]$I, ylim = c(0, 0.3), ylab = 'Infected')
for(i in 1:10){
  lines(brazil_sir_out[[i]]$I)
}
lines(brazil_sir_out[['iter1_13C_15d_peak']]$I, ylab = 'Infected', col = 'darkred')
lines(brazil_sir_out[['iter1_5C_5d_peak']]$I, ylim = c(10, 40), col = 'purple')
lines(brazil_sir_out[['iter1_1C_3d_peak']]$I, col = 'red')
lines(brazil_sir_out[['iter10_13C_15d_peak']]$I, ylab = 'Infected', col = 'darkred')


par(mfrow = c(2, 1), mar = c(4,4, 0.2, 0.2))
plot.ts(TS_BANGLADESH[['iter7_13C_15d_peak']], ylab = 'Temperature', ylim = c(15, 50), col = 'darkred', xaxt = 'n', xlab = '')
lines(TS_BANGLADESH[['iter7_5C_5d_peak']], ylab = 'Temperature', ylim = c(10, 40), col = 'purple')
lines(TS_BANGLADESH[['iter7_1C_3d_peak']], ylab = 'Temperature', ylim = c(10, 40), col = 'red')
lines(TS_BANGLADESH[["iter7_normal"]], ylim = c(10, 40), ylab = 'Temperature')

plot.ts(bangladesh_sir_out[['iter1_13C_15d_peak']]$I, ylab = 'Infected', col = 'darkred')
lines(bangladesh_sir_out[['iter1_5C_5d_peak']]$I, ylim = c(10, 40), col = 'purple')
lines(bangladesh_sir_out[['iter1_1C_3d_peak']]$I, col = 'red')
lines(bangladesh_sir_out[["iter1_normal"]]$I)




generate_climate_with_trend <- function(climate_mean, amplitude, yearly_trend, t, variability_coef) {
  error <- rnorm(t, mean = 0, sd = variability_coef * t / 365)
  climate_mean + amplitude * sin((2 * pi) / 365 * t) + (yearly_trend * t / 365) + error
}

# Example usage
set.seed(123)  # Set seed for reproducibility
t <- 1000  # Number of time steps (assuming one year for simplicity)
climate_mean <- 10  # Mean climate value
amplitude <- 5  # Amplitude of the sinusoidal component
yearly_trend <- 2  # Yearly trend coefficient
variability_coef <- 0.2  # Coefficient for controlling variability

climate_data <- generate_climate_with_trend(climate_mean, amplitude, yearly_trend, seq(1, t), variability_coef)
plot.ts(climate_data)



simulate_daily_rainfall <- function(num_days, avg_days_with_rain_per_month, mean_total_rainfall_per_month) {
  # Initialize daily rainfall vector
  rainfall <- numeric(num_days)
  
  # Validate input vectors
  if (length(avg_days_with_rain_per_month) != 12 || length(mean_total_rainfall_per_month) != 12) {
    stop("Input vectors must have 12 values, one for each month.")
  }
  
  # Loop through each day in the simulation
  for (day in 1:num_days) {
    # Determine the current month
    current_month <- ((day - 1) %% 360) %/% 30 + 1
    
    # Ensure non-negative values for avg_days_with_rain_per_month
    avg_days_with_rain <- max(0, avg_days_with_rain_per_month[current_month])
    
    # Generate random number of rainy days for the month using rpois
    rainy_days <- max(0, rpois(1, lambda = avg_days_with_rain))
    
    print(rainy_days)
    # Check if it's a rainy day
    if (is.finite(rainy_days) && rainy_days > 0) {
      # Ensure non-negative values for mean_total_rainfall_per_month
      mean_rainfall <- max(0, mean_total_rainfall_per_month[current_month] / rainy_days)
      
      # Generate random daily rainfall for each rainy day
      if (runif(1) < 1 / rainy_days) {
        rainfall[day] <- rnorm(1, mean = mean_rainfall, sd = mean_rainfall * 0.10)
      }
    }
  }
  
  return(rainfall)
}

# Set the number of days in the simulation
num_days <- 365 * 3  # Example: simulate for 2 years

# Set vectors for average days with rain and mean total rainfall per month
avg_days_with_rain_per_month <- c(3,5,7,11,13,8,7,11,12,12,7,3)  # Replace with desired values
mean_total_rainfall_per_month <- c(30,35,75,155,190,110,65,135,165,160,80,45)  # Replace with desired values

# Simulate daily rainfall for any length of time
rainfall_data <- simulate_daily_rainfall(num_days, avg_days_with_rain_per_month, mean_total_rainfall_per_month)

# Plot the simulated rainfall data
plot(rainfall_data, type = 'l', xlab = 'Day', ylab = 'Rainfall (mm)', main = 'Simulated Daily Rainfall')

r2 <- generate_extreme_event(x = rainfall_data
                             , magnitude_change = 20
                             , time1 = t1
                             , time2 = t2
                             , duration = 11
                             , timing = 'peak'
)


install.packages('openmeteo')
library('openmeteo')
x<- weather_history("Port-au-Prince",
                    start = "2020-01-01",
                    end = "2021-12-31",
                    hourly = "temperature_2m"
)
plot.ts(x$hourly_temperature_2m)



detect_anomalies <- function(values, percentile = 90) {
  # Calculate the percentile
  threshold <- quantile(values, probs = percentile/100)

  # Calculate the mean for the whole vector
  mean_value <- mean(values)
  
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
      intensity <- mean(anomaly_values)
      duration <- length(anomaly_values)

      anomalies_df <- rbind(anomalies_df, data.frame(Start = start_index, End = end_index, Max_Value = max_value, Intensity = intensity, Duration = duration))
    }
  }
  
  return(anomalies_df)
}

# Example usage:
set.seed(123)
values <- rnorm(10000)
anomalies_result <- detect_anomalies(values = x1$X0, percentile = 90)
print(anomalies_result)


prob_ee <- function(df, timespan){
  df$Intensity <- round(df$Intensity)
  df <- df %>%
    group_by(Duration, Intensity) %>%
      summarise(ee_count = length(Intensity),
                prob = ee_count / timespan)
  df
}

eeprobs <- prob_ee(df = anomalies_result, timespan = nrow(x1))
