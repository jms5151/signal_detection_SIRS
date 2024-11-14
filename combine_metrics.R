# Load required libraries
library(GGally)
library(ggplot2)
library(tidyverse)

# read in data
x <- readRDS('../data/sim_summaries/long_summary.RData')

# format
x$Dataset <- ifelse(
  grepl('^normal', x$List),
  gsub("^[^_]*_[^_]*_([^_]*)_?.*", "\\1", x$List),
  gsub("^[^_]*_([^_]*)_.*", "\\1", x$List)
)

x$ee <- ifelse(x$experiment == 'control', 'control', x$ee)

# remove duplicted rows (controls were duplicted for power analysis)
x <- x[!duplicated(x), ]

# Normalize metrics by regime, susceptibility start
normalized_df <- x %>% 
  group_by(filename, ee) %>%
  mutate(
    peak_timing = as.vector(scale(peak_timing)),
    max_incidence = as.vector(scale(max_incidence)),
    cumulative_proportion = as.vector(scale(cumulative_proportion)),
    duration = as.vector(scale(duration))#,
    # peakiness = scale(peakiness)
    ) %>%
  as.data.frame()

# restructure from long to wide (columns = ids, control, scenarios)
spread_by_metric <- function(df, metric){
  ndf2 <- df[c('Dataset', 'filename', 'ee', metric)] %>%
    spread(key = ee, value = metric)
  return(ndf2)
}

# calculate squared differences
calc_sq_diff <- function(df){
  perturbationColumns <- colnames(df)[!grepl('Dataset|filename|control', colnames(df))]
  df[perturbationColumns] <- (df[perturbationColumns]-df$control)^2
  return(df)
}

# may need to go from wide to long again for plotting purposes
gather_by_metric <- function(df, metric){
  df2 <- df %>%
    select(-control) %>%
    gather(key = ee, value = squared_diff, -c(Dataset, filename))
  colnames(df2)[ncol(df2)] <- paste0(metric, '_squared_diff')
  return(df2)
}

# combine across metrics
wbd_group <- normalized_df[grepl('dry|moderate|wet', normalized_df$filename), ]
vbd_group <- normalized_df[grepl('temperate|warm|hot', normalized_df$filename), ]

metricNames <- c('peak_timing', 'max_incidence', 'cumulative_proportion', 'duration')

process_group <- function(df){
  for(i in 1:length(metricNames)){
    df1 <- spread_by_metric(df = df, metric = metricNames[i])
    df2 <- calc_sq_diff(df = df1)
    df3 <- gather_by_metric(df = df2, metric = metricNames[i])
    if(i == 1){
      new_df <- df3
    } else {
      new_df <- left_join(new_df, df3)
    }
  }
  return(new_df)
}

wbd_sq_diff <- process_group(df = wbd_group)
vbd_sq_diff <- process_group(df = vbd_group)

# for plotting
longCombined <- rbind(wbd_sq_diff, vbd_sq_diff)

# subgroups
s_groups <- unique(longCombined$filename)

# pair plot function
create_plots <- function(sName){
  # subset data
  xx <- subset(longCombined, filename == sName)
  # pair plots
  p <- ggpairs(xx[,4:7]) + theme_bw() + ggtitle(sName)
  return(p)
}

# create and save all pair plots
# This isn't exactly correct, what we want is the difference!
for(j in s_groups){
    p <- create_plots(sName = j)
    ggsave(filename = paste0('../figures/pair_plots/', j, '.pdf'), plot = p, width = 9, height = 6)
}


# calculate combined values
distance_calc <- function(df, xcols){
  sqrt(rowSums(df[,xcols]))
}

# Max incidence & cumulative proportion
longCombined$MaxInc_CumProp <- distance_calc(df = longCombined, xcols = c('max_incidence_squared_diff', 'cumulative_proportion_squared_diff'))

# Cumulative proportion & duration
longCombined$CumProp_Dur <- distance_calc(df = longCombined, xcols = c('cumulative_proportion_squared_diff', 'duration_squared_diff'))

# Max incidence, cumulative proportion, & duration
longCombined$MaxInc_CumProp_Dur <- distance_calc(df = longCombined, xcols = c('max_incidence_squared_diff', 'cumulative_proportion_squared_diff', 'duration_squared_diff'))

# Peak timing & max incidence
longCombined$PeakT_MaxInc <- distance_calc(df = longCombined, xcols = c('peak_timing_squared_diff', 'max_incidence_squared_diff'))

# Peak timing & cumulative proportion
longCombined$PeakT_CumProp <- distance_calc(df = longCombined, xcols = c('peak_timing_squared_diff', 'cumulative_proportion_squared_diff'))

# Peak timing, max incidence, & cumulative proportion
longCombined$PeakT_MaxInc_CumProp <- distance_calc(df = longCombined, xcols = c('peak_timing_squared_diff', 'max_incidence_squared_diff', 'cumulative_proportion_squared_diff'))

# t tests (are combined values significantly different from zero)

# Define the columns to test
test_columns <- c('MaxInc_CumProp'
                  , 'CumProp_Dur'
                  , 'MaxInc_CumProp_Dur'
                  , 'PeakT_MaxInc'
                  , 'PeakT_CumProp'
                  , 'PeakT_MaxInc_CumProp')

# Perform t.test on multiple columns within groups
results <- longCombined %>%
  group_by(filename, ee) %>%
  summarize(
    across(
      all_of(test_columns),
      ~ list(t.test(.x, alternative = "greater")), # Apply t.test
      .names = "t_test_{.col}"
    ),
    .groups = "drop"
  ) %>%
  # Dynamically extract p.value and statistic for all test columns
  mutate(across(
    starts_with("t_test_"),
    list(
      p_value = ~ map_dbl(.x, ~ .x$p.value),
      statistic = ~ map_dbl(.x, ~ .x$statistic)
    ),
    .names = "{.fn}_{.col}"
  )) %>%
  # Clean up: Remove raw t.test columns
  select(filename, ee, matches("p_value|statistic"))

# View the final table
print(results)

# find highest statistic.
# Perhaps better to go long, remove rows with p-values >= 0.01, then take highest?
# reshape to long data
results_long <- results %>%
  gather(key = statistic_name, value = 'value', -c(filename, ee))

# format
results_long$statistic <- ifelse(grepl('^p_value', results_long$statistic_name), 'p_value', 't_statistic')
results_long$combined_metric <- gsub('statistic_|p_value_|t_test_', '',  results_long$statistic_name)
  
# separate and combine
rl_p <- subset(results_long, statistic == 'p_value')
colnames(rl_p)[4] <- 'p_value' 
rl_t <- subset(results_long, statistic == 't_statistic')
colnames(rl_t)[4] <- 't_statistic' 
rl2 <- rl_p[,c('filename', 'ee', 'p_value', 'combined_metric')] %>% 
  left_join(rl_t[,c('filename', 'ee', 't_statistic', 'combined_metric')])

# remove rows with significance values above 0.01
rl2 <- subset(rl2, p_value <= 0.01)

# identify metric with highest t-statistic
highest_t <- rl2 %>%
  group_by(filename, ee) %>%
  top_n(1, t_statistic) %>%
  # select(filename, ee, metric, power)
  select(filename, ee, combined_metric, p_value, t_statistic) %>%
  as.data.frame()

# save results
saveRDS(highest_t, file = '../data/sim_summaries/highest_t_summary.RData')

