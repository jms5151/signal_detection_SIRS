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

# subgroups
s_groups <- unique(normalized_df$filename)

# pair plot function
create_plots <- function(sName){
  # subset data
  xx <- subset(normalized_df, filename == sName)
  # pair plots
  p <- ggpairs(xx[,3:6]) + theme_bw() + ggtitle(sName)
  return(p)
}

# create and save all pair plots
for(j in s_groups){
    p <- create_plots(sName = j)
    ggsave(filename = paste0('../figures/pair_plots/', j, '.pdf'), plot = p, width = 9, height = 6)
}


