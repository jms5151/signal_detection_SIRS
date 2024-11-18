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

# # subgroups
# s_groups <- unique(longCombined$filename)
# 
# # pair plot function
# create_plots <- function(sName){
#   # subset data
#   xx <- subset(longCombined, filename == sName)
#   # pair plots
#   p <- ggpairs(xx[,paste0(metricNames, '_diff')]) + theme_bw() + ggtitle(sName)
#   return(p)
# }
# 
# # create and save all pair plots
# # This isn't exactly correct, what we want is the difference!
# for(j in s_groups){
#     p <- create_plots(sName = j)
#     ggsave(filename = paste0('../figures/pair_plots/', j, '.pdf'), plot = p, width = 9, height = 6)
# }


longCombined[, paste0(metricNames, '_abs_diff')] <- lapply(longCombined[,paste0(metricNames, '_diff')], function(x) abs(x))

# calculate combined values
distance_calc <- function(df, xcols){
  df[,xcols] <- lapply(df[,xcols], function(x) x^2)
  sqrt(rowSums(df[,xcols]))
}

# Max incidence & cumulative proportion
longCombined$MaxInc_CumProp <- distance_calc(df = longCombined, xcols = c('max_incidence_diff', 'cumulative_proportion_diff'))

# Cumulative proportion & duration
longCombined$CumProp_Dur <- distance_calc(df = longCombined, xcols = c('cumulative_proportion_diff', 'duration_diff'))

# Max incidence, cumulative proportion, & duration
longCombined$MaxInc_CumProp_Dur <- distance_calc(df = longCombined, xcols = c('max_incidence_diff', 'cumulative_proportion_diff', 'duration_diff'))

# Peak timing & max incidence
longCombined$PeakT_MaxInc <- distance_calc(df = longCombined, xcols = c('peak_timing_diff', 'max_incidence_diff'))

# Peak timing & cumulative proportion
longCombined$PeakT_CumProp <- distance_calc(df = longCombined, xcols = c('peak_timing_diff', 'cumulative_proportion_diff'))

# Peak timing, max incidence, & cumulative proportion
longCombined$PeakT_MaxInc_CumProp <- distance_calc(df = longCombined, xcols = c('peak_timing_diff', 'max_incidence_diff', 'cumulative_proportion_diff'))

# Peak timing, cumulative proportion, & duration
longCombined$PeakT_CumProp_Dur <- distance_calc(df = longCombined, xcols = c('peak_timing_diff', 'cumulative_proportion_diff', 'duration_diff'))

# Peak timing, max incidence, cumulative proportion, & cumulative proportion
longCombined$all_combined <- distance_calc(df = longCombined, xcols = c('peak_timing_diff', 'max_incidence_diff', 'cumulative_proportion_diff', 'duration_diff'))

# t tests (are combined values significantly different from zero)

# Define the columns to test
test_columns <- c(
  # 'peak_timing_abs_diff'
  # , 'max_incidence_abs_diff'
  # , 'cumulative_proportion_abs_diff'
  # , 'duration_abs_diff'
  # , 'MaxInc_CumProp'
  #                 ,  'CumProp_Dur'
  #                 , 'MaxInc_CumProp_Dur'
  #                 , 'PeakT_MaxInc'
  #                 , 'PeakT_CumProp'
  #                 , 'PeakT_MaxInc_CumProp'
                   #,  'all_combined'
  'PeakT_CumProp_Dur'
                  )

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

# remove rows with significance values above 0.01: 87% significant
# maybe we shouldn't do this and put points where significant
# rl2 <- subset(rl2, p_value <= 0.01)

# oneMetricRL2 <- subset(rl2, combined_metric == paste0(metricNames, '_abs_diff')) #[rl2$combined_metric == paste0(metricNames, '_abs_diff')),]

# identify metric with highest t-statistic
highest_t <- rl2 %>%
  drop_na() %>%
  group_by(filename, ee) %>%
  # slice(which.min(p_value)) %>%
  top_n(1, t_statistic) %>%
  select(filename, ee, combined_metric, p_value, t_statistic) %>%
  as.data.frame()

# save results
saveRDS(highest_t, file = '../data/sim_summaries/highest_t_summary.RData')

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


# combos <- longCombined %>%
#   group_by(filename, ee) %>%
#   reframe(
#     QDPT_greater = sum(qual_diff_peak_timing == 'greater')
#     , QDPT_less = sum(qual_diff_peak_timing == 'less')
#     , QDPT_num = length(qual_diff_peak_timing)
#     , QDPT = ifelse(QDPT_greater >= 0.75 * QDPT_num, 'greater', NA)
#     , QDPT = ifelse(QDPT_less >= 0.75 * QDPT_num, 'less', QDPT)
#     , QDPT = ifelse(is.na(QDPT), 'same', QDPT)
#     
#     , QDMI_greater = sum(qual_diff_max_incidence == 'greater')
#     , QDMI_less = sum(qual_diff_max_incidence == 'less')
#     , QDMI = ifelse(QDMI_greater > QDMI_less, 'greater', 'less')
# 
#     , QDCP_greater = sum(qual_diff_cumulative_proportion == 'greater')
#     , QDCP_less = sum(qual_diff_cumulative_proportion == 'less')
#     , QDCP = ifelse(QDCP_greater > QDCP_less, 'greater', 'less')
# 
#     , QDD_greater = sum(qual_diff_duration == 'greater')
#     , QDD_less = sum(qual_diff_duration == 'less')
#     , QDD = ifelse(QDD_greater > QDD_less, 'greater', 'less')
#     
#     , qual_MaxInc_CumProp = paste0(QDMI, '_MaxInc_', QDCP, '_CumProp')
#     , qual_CumProp_Dur = paste0(QDCP, '_CumProp_', QDD, '_Dur')
#     , qual_MaxInc_CumProp_Dur = paste0(QDMI, '_MaxInc_', QDCP, '_CumProp_', QDD, '_Dur')
#     , qual_PeakT_MaxInc = paste0(QDPT, '_PeakT_', QDMI, '_MaxInc')
#     , qual_PeakT_CumProp = paste0(QDPT, '_PeakT_', QDCP, '_CumProp')
#     , qual_PeakT_MaxInc_CumProp = paste0(QDPT, '_PeakT_', QDMI, '_MaxInc_', QDCP, '_CumProp')
#     , qual_all_combined = paste0(QDPT, '_PeakT_', QDMI, '_MaxInc_', QDCP, '_CumProp_', QDD, '_Dur')
#   )

combosLong <- combos2 %>%
  select(-c(colnames(combos)[grepl('^QD', colnames(combos))])) %>%
  gather(key = combined_metric, value = 'direction', -c(filename, ee) )
combosLong$combined_metric <- gsub('qual_', '', combosLong$combined_metric)
  
# combine
# highest_t_combos <- highest_t %>% left_join(combosLong)
highest_t_combos <- rl2 %>% left_join(combosLong)

# format columns
highest_t_combos$suscept <- gsub('_t.*', '', highest_t_combos$filename)
highest_t_combos$regime <- gsub('.*_t_', '', highest_t_combos$filename)
highest_t_combos$intensity <- gsub('_.*', '', highest_t_combos$ee)
highest_t_combos$intensity <- gsub('I', '', highest_t_combos$intensity)
highest_t_combos$intensity <- as.numeric(highest_t_combos$intensity)
highest_t_combos$duration <- gsub('.*_', '', highest_t_combos$ee)
highest_t_combos$duration <- gsub('D', '', highest_t_combos$duration)
highest_t_combos$duration <- as.numeric(highest_t_combos$duration)


x = highest_t_combos

x$regime <- factor(x$regime, levels = c('temperate', 'warm', 'hot', 'dry', 'moderate', 'wet'))

# rename direction to something interpret able
x$direction[x$direction == 'same_PeakT_same_CumProp_less_Dur'] <- 'Shorter outbreak'
x$direction[x$direction == 'greater_PeakT_same_CumProp_less_Dur'] <- 'Later and shorter outbreak'
x$direction[x$direction == 'less_PeakT_same_CumProp_same_Dur'] <- 'Earlier outbreak'
x$direction[x$direction == 'same_PeakT_same_CumProp_same_Dur'] <- 'No difference'
x$direction[x$direction == 'greater_PeakT_greater_CumProp_greater_Dur'] <- 'Later, larger, and longer outbreak'
x$direction[x$direction == 'less_PeakT_greater_CumProp_greater_Dur'] <- 'Earlier, larger, and longer outbreak'
x$direction[x$direction == 'less_PeakT_same_CumProp_greater_Dur'] <- 'Earlier and longer outbreak'

x$direction[x$direction == 'less_PeakT_same_CumProp_less_Dur'] <- 'Earlier and shorter outbreak'
x$direction[x$direction == 'same_PeakT_less_CumProp_less_Dur'] <- 'Smaller and shorter outbreak'
x$direction[x$direction == 'same_PeakT_same_CumProp_greater_Dur'] <- 'Longer outbreak'
x$direction[x$direction == 'greater_PeakT_same_CumProp_same_Dur'] <- 'Later outbreak'
x$direction[x$direction == 'same_PeakT_greater_CumProp_greater_Dur'] <- 'Larger and longer outbreak'
x$direction[x$direction == 'greater_PeakT_same_CumProp_greater_Dur'] <- 'Later and longer outbreak'
x$direction[x$direction == 'less_PeakT_greater_CumProp_same_Dur'] <- 'Earlier and larger outbreak'

x$direction[x$direction == 'same_PeakT_greater_CumProp_same_Dur'] <- 'Larger outbreak'
x$direction[x$direction == 'greater_PeakT_less_CumProp_less_Dur'] <- 'Later, smaller, and shorter outbreak'
x$direction[x$direction == 'greater_PeakT_greater_CumProp_same_Dur'] <- 'Later and larger outbreak'
x$direction[x$direction == 'same_PeakT_less_CumProp_same_Dur'] <- 'Smaller outbreak'
x$direction[x$direction == 'less_PeakT_less_CumProp_less_Dur'] <- 'Earlier, smaller, and shorter outbreak'
x$direction[x$direction == 'less_PeakT_greater_CumProp_less_Dur'] <- 'Earlier, larger, and shorter outbreak'


x2 = subset(x, regime == 'dry'| regime == 'moderate' | regime == 'wet')
x2 = subset(x, regime == 'temperate'| regime == 'warm' | regime == 'hot')

ggplot() +
  # First, the geom_tile layer with fill for `metric`
  geom_tile(data = x2, aes(x = intensity, y = duration, fill = direction)) +  # Removes borders around the tiles
  # scale_alpha_identity() +
  
  # Use scale_alpha_continuous for continuous alpha mapping
  # scale_alpha_continuous(range = c(0.25, 1)) +  # Adjust the range if necessary
  
  # Apply the correct scale based on metric type
  scale_fill_viridis_d() +
  
  # Second, the geom_point layer with shape for `power_indicator`
  # geom_point(data = stdata, aes(x = intensity, y = duration, shape = power_indicator), 
  #            size = stpsize, color = 'black', alpha = 0.5) +  # Stippling effect
  
  theme_bw() +
  facet_grid(suscept~ regime, scales = 'free') + # suscept 
  xlab('Intensity') +
  ylab('Duration') +
  labs(fill = '', shape = '') +  # Separate labels for shape and fill
  
  # Remove gridlines for cleaner presentation
  theme(legend.position = 'bottom',
        panel.grid.major = element_blank(),   # Removes major gridlines
        panel.grid.minor = element_blank()) +
  
  # Use guides to separate fill and shape legends
  guides(fill = guide_legend(order = 1),      # Control order of legends
         shape = guide_legend(order = 2),
         alpha = 'none')     # Display shape legend separately

