# Load required libraries
library(GGally)
library(tidyverse)
library(stringr)
library(ggplot2)
library(patchwork)

# read in data
x <- readRDS('../data/sim_summaries/long_summary.RData')

# format
x$Dataset <- ifelse(
  grepl('^normal', x$List),
  gsub("^[^_]*_[^_]*_([^_]*)_?.*", "\\1", x$List),
  gsub("^[^_]*_([^_]*)_.*", "\\1", x$List)
)

x$ee <- ifelse(x$experiment == 'control', 'control', x$ee)

# remove duplicated rows (controls were duplicted for power analysis)
x <- x[!duplicated(x), ]

# Normalize metrics by regime, susceptibility start
normalized_df <- x %>% 
  group_by(filename, ee) %>%
  mutate(
    peak_timing = as.vector(scale(peak_timing))
    , max_incidence = as.vector(scale(max_incidence))
    , cumulative_proportion = as.vector(scale(cumulative_proportion))
    , duration = as.vector(scale(duration))
    ) %>%
  as.data.frame()

# restructure from long to wide (columns = ids, control, scenarios)
spread_by_metric <- function(df, metric){
  ndf2 <- df[c('Dataset', 'filename', 'ee', metric)] %>%
    spread(key = ee, value = metric)
  return(ndf2)
}

# calculate squared differences
calc_diff <- function(df){
  perturbationColumns <- colnames(df)[!grepl('Dataset|filename|control', colnames(df))]
  df[perturbationColumns] <- df[perturbationColumns]-df$control
  df[paste0('qual_', perturbationColumns)] <- ifelse(df[perturbationColumns] <= 0, 'less', 'greater')
  return(df)
}

# may need to go from wide to long again for plotting purposes
gather_by_metric <- function(df, metric){
  df2 <- df %>%
    select(-control) %>%
    gather(key = ee, value = diff, -c(Dataset, filename))
  colnames(df2)[ncol(df2)] <- paste0(metric, '_diff')
  return(df2)
}

# combine across metrics
wbd_group <- normalized_df[grepl('dry|moderate|wet', normalized_df$filename), ]
vbd_group <- normalized_df[grepl('temperate|warm|hot', normalized_df$filename), ]

metricNames <- c('peak_timing', 'max_incidence', 'cumulative_proportion', 'duration')

process_group <- function(df){
  for(i in 1:length(metricNames)){
    df1 <- spread_by_metric(df = df, metric = metricNames[i])
    df2 <- calc_diff(df = df1)
    df3 <- gather_by_metric(df = df2, metric = metricNames[i])
    df31 <- df3[!grepl('^qual_', df3$ee),]
    df32 <- df3[grepl('^qual_', df3$ee),]
    colnames(df32)[4] <- paste0('qual_diff_', metricNames[i]) 
    df32$ee <- gsub('qual_', '', df32$ee)
    df4 <- df31 %>% left_join(df32)
    if(i == 1){
      new_df <- df4
    } else {
      new_df <- left_join(new_df, df4)
    }
  }
  return(new_df)
}

wbd_sq_diff <- process_group(df = wbd_group)
vbd_sq_diff <- process_group(df = vbd_group)

# for plotting
longCombined <- rbind(wbd_sq_diff, vbd_sq_diff)

# format numeric columns
longCombined[paste0(metricNames, '_diff')] <- lapply(longCombined[paste0(metricNames, '_diff')], as.numeric)

# calculate combined values
distance_calc <- function(df, xcols){
  df[,xcols] <- lapply(df[,xcols], function(x) x^2)
  sqrt(rowSums(df[,xcols]))
}

# Peak timing, cumulative proportion, & duration
longCombined$PeakT_CumProp_Dur <- distance_calc(df = longCombined, xcols = c('peak_timing_diff', 'cumulative_proportion_diff', 'duration_diff'))

# Define the columns to test
test_columns <- c('PeakT_CumProp_Dur')

# Perform t.test by groups
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


# find highest statistic
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

# determine less than/greater than combinations
combos <- longCombined %>%
  group_by(filename, ee) %>%
  summarise(
    across(
      starts_with("qual_diff_"), 
      list(
        greater = ~ sum(. == "greater"),
        less = ~ sum(. == "less"),
        num = ~ length(.),
        category = ~ case_when(
          sum(. == "greater") >= 0.8 * length(.) ~ "greater",
          sum(. == "less") >= 0.8 * length(.) ~ "less",
          TRUE ~ "same"
        )
      ),
      .names = "{col}_{fn}"
    ),
    .groups = "drop"
  )

combos2 <- combos %>%
  group_by(filename, ee) %>%
  reframe(
    qual_PeakT_CumProp_Dur = paste0(
          qual_diff_peak_timing_category, '_PeakT_'
          # , qual_diff_max_incidence_category, '_MaxInc_'
          , qual_diff_cumulative_proportion_category, '_CumProp_'
          , qual_diff_duration_category, '_Dur')
  )


combosLong <- combos2 %>%
  select(-c(colnames(combos)[grepl('^QD', colnames(combos))])) %>%
  gather(key = combined_metric, value = 'direction', -c(filename, ee) )
combosLong$combined_metric <- gsub('qual_', '', combosLong$combined_metric)
  
# combine
highest_t_combos <- rl2 %>% left_join(combosLong)

# format columns
highest_t_combos$suscept <- gsub('_t.*', '', highest_t_combos$filename)
highest_t_combos$regime <- gsub('.*_t_', '', highest_t_combos$filename)
highest_t_combos$regime <- str_to_sentence(highest_t_combos$regime)
highest_t_combos$intensity <- gsub('_.*', '', highest_t_combos$ee)
highest_t_combos$intensity <- gsub('I', '', highest_t_combos$intensity)
highest_t_combos$intensity <- as.numeric(highest_t_combos$intensity)
highest_t_combos$duration <- gsub('.*_', '', highest_t_combos$ee)
highest_t_combos$duration <- gsub('D', '', highest_t_combos$duration)
highest_t_combos$duration <- as.numeric(highest_t_combos$duration)

# save
saveRDS(highest_t_combos, '../data/sim_summaries/highest_t_summary.RData')

