# load library
library(tidyverse)

# source code to simulate climate
source('functions_to_simulate_climate.R')

# load data
wprecip <- read.csv('../data/wikipedia_precip.csv')
wtemp <- read.csv('../data/wikipedia_temp.csv')
wtemp <-wtemp[wtemp$Country != "",] # remove rows that correspond to temperature in Fahrenheit 

# Calculate Row-wise Min and Max
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

# Calculate 90th percentile values based on sin waves
addp90 <- function(df, clim = 'notrain'){
  df$p90 <- NA
  df$p10 <- NA
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
    df$p90[i] <- round(quantile(s, 0.90))
    df$p10[i] <- round(quantile(s, 0.10))
  }
  return(as.data.frame(df))  
}

# apply to temperature data
wtemp <- add_monthly_values(df = wtemp)
wtemp <- addp90(df = wtemp)

# apply to precipitation data
wprecip <- add_monthly_values(df = wprecip)
wprecip <- addp90(df = wprecip)

# subset data
t28 <- subset(wtemp, p90 == 28)
t24 <- subset(wtemp, p90 == 24)
t20 <- subset(wtemp, p90 == 20)

dry <- subset(wprecip, p90 > 95 & p90 < 115)
mod <- subset(wprecip, p90 > 150 & p90 < 170)
wet <- subset(wprecip, p90 > 330 & p90 < 350)
