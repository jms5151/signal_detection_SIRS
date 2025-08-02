# validation

# Load libraries
library(ncdf4)
library(tidyverse)
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)

# Load outbreak data
outbreaks <- read_csv('../outbreaks.csv') %>%
  mutate(across(ends_with('date'), as.Date))

# update dates
outbreaks_fixed <- outbreaks %>%
  mutate(
    Date_start = if_else(is.na(Date_start), Date_peak, Date_start),
    Date_end   = if_else(is.na(Date_end), Date_peak, Date_end)
  )

# List climate model NetCDFs
nc_files <- list.files('../data/climate_models/', pattern = '\\.nc$', full.names = TRUE)

# Define your beta transformation function
# source('functions_to_calculate_beta.R')
Eisenberg_beta <- function(rainfall) {
  0.243 + (0.00128 * rainfall)
}

# analyze data
analyze_outbreak_model <- function(nc_path, outbreak_df) {
  model_name <- basename(nc_path)
  
  # Open and load data
  nc <- nc_open(nc_path)
  on.exit(nc_close(nc))
  
  precip <- ncvar_get(nc, 'precip')     # [City, time]
  time_raw <- ncvar_get(nc, 'time')
  time_dates <- as.Date(floor(time_raw), origin = '0001-01-01')  # safer truncation
  
  # Identify post-1980 indices and extract associated years
  after_1980 <- which(time_dates >= as.Date('1980-01-01'))
  years <- as.numeric(format(time_dates[after_1980], '%Y'))
  
  outbreak_df %>%
    rowwise() %>%
    mutate(
      # Padded date window
      date_range = list(which(time_dates >= Date_start - 1 & time_dates <= Date_end + 1)),
      
      # Safely get peak index from range
      peak_index = {
        date_vec <- date_range[[1]]
        date_vec[which.max(precip[id, date_vec])]
      },
      
      # Extract rainfall and compute metrics
      peak_rain = precip[id, peak_index],
      rain_dist = list(precip[id, after_1980]),
      
      prob_rain = mean(rain_dist >= peak_rain, na.rm = TRUE),
      peak_beta = Eisenberg_beta(peak_rain),
      beta_dist = Eisenberg_beta(rain_dist[[1]]),
      prob_beta = mean(beta_dist >= peak_beta, na.rm = TRUE),
      rain_90th = quantile(rain_dist[[1]], probs = 0.90, na.rm = TRUE),
      
      # Annual rainfall totals + average
      rain_by_year = list(tapply(precip[id, after_1980], years, sum, na.rm = TRUE)),
      avg_annual_rain = mean(rain_by_year[[1]], na.rm = TRUE),
      
      model = model_name
    ) %>%
    ungroup() %>%
    select(model, id, Date_start, Date_end,
           peak_rain, prob_rain, peak_beta, prob_beta, rain_90th, avg_annual_rain)
}


# Apply the analysis function to all nc files
all_results <- map_dfr(nc_files, analyze_outbreak_model, outbreak_df = outbreaks_fixed)

# Label scenario from model name
all_results <- all_results %>%
  mutate(scenario = ifelse(grepl('rmWarming', model), 'baseline', 'exposure'))

# Bootstrap RR and FAR per outbreak
bootstrap_summary <- function(df, n_boot = 1000) {
  df %>%
    group_by(id) %>%
    filter(all(c('baseline', 'exposure') %in% scenario)) %>%
    group_modify(~{
      base <- .x %>% filter(scenario == 'baseline') %>% pull(peak_beta)
      exp <- .x %>% filter(scenario == 'exposure') %>% pull(peak_beta)
      
      boot <- tibble(
        RR  = replicate(n_boot, {
          b <- sample(base, replace = TRUE)
          e <- sample(exp, replace = TRUE)
          mean(e, na.rm = TRUE) / mean(b, na.rm = TRUE)
        }),
        FAR = replicate(n_boot, {
          b <- sample(base, replace = TRUE)
          e <- sample(exp, replace = TRUE)
          1 - mean(b, na.rm = TRUE) / mean(e, na.rm = TRUE)
        }),
        peak_beta = replicate(n_boot, {
          mean(base, replace = TRUE)
        })
      )
      
      tibble(
        RR_median = median(boot$RR, na.rm = TRUE),
        RR_lower  = quantile(boot$RR, 0.25, na.rm = TRUE),
        RR_upper  = quantile(boot$RR, 0.75, na.rm = TRUE),
        FAR_median = median(boot$FAR, na.rm = TRUE),
        FAR_lower  = quantile(boot$FAR, 0.25, na.rm = TRUE),
        FAR_upper  = quantile(boot$FAR, 0.75, na.rm = TRUE),
        peak_beta = median(boot$peak_beta, na.rm = TRUE)
      )
    }) %>%
    ungroup()
}

boot_summary <- bootstrap_summary(all_results)

# Step 3: Calculate rainfall deviation and rain_90th per outbreak
rain_summary <- all_results %>%
  filter(scenario == 'exposure') %>%
  group_by(id) %>%
  summarize(
    rain_90th = mean(rain_90th, na.rm = TRUE),
    rain_deviation = mean(peak_rain, na.rm = TRUE) / mean(rain_90th, na.rm = TRUE),
    avg_annual_rain = mean(avg_annual_rain, na.rm = TRUE),
    .groups = 'drop'
  )


# Define custom quantile cutoffs
quantiles <- quantile(rain_summary$avg_annual_rain[outbreaks$Disease == 'Cholera'], probs = c(0.25, 0.75), na.rm = TRUE)

# Categorize cities by annual average rainfall instead!
x2 <- left_join(boot_summary, rain_summary, by = 'id') %>%
  filter(id %in% outbreaks$id[outbreaks$Disease == 'Cholera']) %>%
  mutate(
    rain_cat = case_when(
      avg_annual_rain < quantiles[1] ~ 'Dry',
      avg_annual_rain >= quantiles[1] & avg_annual_rain <= quantiles[2] ~ 'Moderate',
      avg_annual_rain > quantiles[2] ~ 'Wet'
    )
  )

# Calculate theoretical FAR
source('transmission_functions.R')

dry_regime <- median(x2$rain_90th[x2$rain_cat=='Dry'])
moderate_regime <- median(x2$rain_90th[x2$rain_cat=='Moderate'])
wet_regime <- median(x2$rain_90th[x2$rain_cat=='Wet'])

x_range <- seq(1, 15, by = 0.1)

far_by_regime <- function(clim_value){
  x_range_new <- clim_value * x_range
  p0_cholera <- linear_function(x = clim_value, a = 0.00128, b = 0.243)
  p1s_cholera <- linear_function(x = x_range, a = 0.00128, b = 0.243)
  out <- calc_FAR(p0 = p0_cholera, p1 = p1s_cholera)
  return(out)
}

dry_far <- far_by_regime(clim_value = dry_regime)
mod_far <- far_by_regime(clim_value = moderate_regime)
wet_far <- far_by_regime(clim_value = wet_regime)

line_df <- data.frame(
  x = x_range,
  Dry = dry_far,
  Moderate = mod_far,
  Wet = wet_far
)

# Convert to long format
line_df_long <- pivot_longer(line_df, cols = -x, names_to = 'rain_cat', values_to = 'FAR_fit')

# Plot theoretical with estimated FAR for historical cholera outbreaks 
far_cholera_plot <- ggplot(x2, aes(x = rain_deviation, y = FAR_median, color = rain_cat)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = FAR_lower, ymax = FAR_upper), width = 0.05) +
  geom_line(data = line_df_long, aes(x = x, y = FAR_fit, color = rain_cat), size = 1) +
  labs(
    x = 'Relative Ratio (total rainfall / exceedance threshold)',
    y = 'FAR for prior cholera outbreaks',
    # title = "Beta FAR vs Rainfall Deviation",
    color = 'Rainfall Regime'
  ) +
  theme_bw(base_size = 13) +
  theme(legend.position = c(0.8, 0.8), legend.background = element_rect(fill = "transparent", color = NA)) +
  scale_color_manual(
    values = c(
      'Dry' = "brown",
      'Moderate' = "lightblue",
      'Wet' = "blue"
    )
  ) +
  xlim(0,15) +
  ylim(0,0.12)

ggsave(filename = '../figures/historical_cholera_far.pdf', plot = far_cholera_plot, width = 8, height = 6)
