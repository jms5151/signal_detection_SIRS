# load library
library(tidyverse)

# source code to simulate climate
source('functions_to_simulate_climate.R')

# load data
wprecip <- read.csv('../data/wikipedia_precip.csv')
wtemp <- read.csv('../data/wikipedia_temp.csv')

# format
wprecip$Year <- gsub(',', '', wprecip$Year)
wprecip$Year <- as.numeric(wprecip$Year)
wtemp <-wtemp[wtemp$Country != "",] # remove rows that correspond to temperature in Fahrenheit 

# Calculate Row-wise Min and Max
add_monthly_values <- function(df){
  df <- df %>%
    rowwise() %>%
    mutate(
      Min_Month_Value = min(c_across(Jan:Dec), na.rm = TRUE),
      Max_Month_Value = max(c_across(Jan:Dec), na.rm = TRUE),
      # Range = Max_Month_Value - Min_Month_Value
    ) %>%
    ungroup()
  return(df)
}


# apply to temperature data
wtemp <- add_monthly_values(df = wtemp)

# apply to precipitation data
wprecip <- add_monthly_values(df = wprecip)

# subset data
t20 <- subset(wtemp, round(Year) == 20)
t24 <- subset(wtemp, round(Year) == 24)
t28 <- subset(wtemp, round(Year) == 28)

dry <- subset(wprecip, Year <= quantile(wprecip$Year, 0.10))
mod <- subset(wprecip, Year >= quantile(wprecip$Year, 0.45) & Year <= quantile(wprecip$Year, 0.55))
wet <- subset(wprecip, Year >= quantile(wprecip$Year, 0.90))

