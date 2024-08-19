# load libraries
library(dplyr)
library(purrr)
library(tidyverse)
library(stringr)
library(ggplot2)

# function to stack list elements
stack_list_elements <- function(listName){
  combined_df <- map_df(names(listName), ~ {
    data.frame(listName[[.x]]) %>%
      mutate(simulation = .x
             , time = row_number())
  })
  return(combined_df)  
}

# plotting function
create_clim_disease_plot <- function(x, titleName){
  p <- ggplot(x, aes(x = time, y = value, color = rep)) +
    geom_line() +
    facet_grid(metric ~ climate_regime, scales = 'free_y') +
    theme_bw() +
    xlab('Time') +
    ylab('') +
    theme(legend.position = 'none') +
    ggtitle(titleName)
  return(p)
} 

# filepaths
clim_data_path <- '../data/sim_climate/'
sim_data_path <- '../data/sim_results/'

# list files
clim_files <- list.files(clim_data_path, full.names = T)
dis_files <- list.files(sim_data_path, full.names = T)

# open and stack all data
combined_df <- data.frame()

for(j in 1:length(clim_files)){
  cdf <- readRDS(clim_files[j])
  cdf_long <- stack_list_elements(listName = cdf)
  colnames(cdf_long)[1] <- 'Climate'
  ddf <- readRDS(dis_files[j])
  ddf_long <- stack_list_elements(listName = ddf)
  long_df <- cdf_long %>% 
    left_join(ddf_long) %>%
    select(-c(R, beta)) %>%
    pivot_longer(cols=c(Climate, S, I),names_to = 'metric', values_to = 'value')
  if(any(grepl('W', colnames(long_df)))){
    long_df <- long_df %>%
      select(-W)
  }
  combined_df <- rbind(combined_df, long_df)
}
  
# format data
combined_df$climate_regime <- str_match(combined_df$simulation, "^[^_]*_(.*?)_[^_]*$")[, 2]
combined_df$climate_regime <- str_to_title(combined_df$climate_regime)
combined_df$rep <- str_extract(combined_df$simulation, "[^_]+$")
combined_df$disease <- ifelse(combined_df$climate_regime == 'Dry' | combined_df$climate_regime == 'Moderate' | combined_df$climate_regime == 'Wet', 'Water-borne disease', 'Vector-borne disease')
combined_df$metric[combined_df$metric == 'I'] <- 'Infected (proportion)'
combined_df$metric[combined_df$metric == 'S'] <- 'Susceptible (proportion)'

# plot
wbd_df <- subset(combined_df, disease == 'Water-borne disease')
wbd_plot <- create_clim_disease_plot(x = wbd_df, titleName = 'Water-borne disease')
ggsave(filename = '../figures/time_series/wbd_clim_dis_ts.pdf', plot = wbd_plot, width = 11, height = 7)

vbd_df <- subset(combined_df, disease == 'Vector-borne disease')
vbd_df$climate_regime <- factor(vbd_df$climate_regime, levels = c('Temperate', 'Warm', 'Hot'))
vbd_plot <- create_clim_disease_plot(x = vbd_df, titleName = 'Vector-borne disease')
ggsave(filename = '../figures/time_series/vbd_clim_dis_ts.pdf', plot = vbd_plot, width = 11, height = 7)
