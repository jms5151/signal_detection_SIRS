# load libraries
library(tidyverse)
library(ggplot2)
library(patchwork)

# read in data
dfpow <- readRDS('../data/sim_summaries/highest_power_summary.RData')

# format
prain <- subset(dfpow, regime == 'dry' | regime == 'moderate' | regime == 'wet')
ptemp <- subset(dfpow, regime == 'temperate' | regime == 'warm' | regime == 'hot')

# format data
format_ee_data <- function(dfx, climvar){
  x <- dfx %>%
    mutate(alpha_value = ifelse(power > 0.8, 1, 0.7),
           susceptibility = ifelse(suscept == 'S_max', 'Low pop. susceptibility', 'High pop. susceptibility')
    )
  x$regime <- paste0(toupper(substring(x$regime, 1, 1)), substring(x$regime, 2))
  if(climvar == 'temp'){
    x$regime <- factor(x$regime, levels = c('Temperate', 'Warm', 'Hot'))
  } else {
    x$regime <- factor(x$regime, levels = c('Dry', 'Moderate', 'Wet'))
  }
  x$metric <- paste0(toupper(substring(x$metric, 1, 1)), substring(x$metric, 2))
  x$metric[x$metric == 'cumulative_proportion'] <- 'Total cases'
  x$metric[x$metric == 'peak_timing'] <- 'Timing of outbreak peak'
  x$metric[x$metric == 'outbreak_duration'] <- 'Outbreak duration'
  x$metric[x$metric == 'max_incidence'] <- 'Max incidence'
  x$metric <- gsub('_', ' ', x$metric)
  return(x)  
}


# Create a dataframe for the alpha legend
alpha_legend <- data.frame(
  alpha_value = seq(0, 1, length.out = 100),  # Alpha values from 0.25 to 1
  dummy_x = 1  # A constant x-value to create a bar-like structure
)

# Plot the custom alpha legend as a separate plot
alpha_legend_plot <- ggplot(alpha_legend, aes(x = dummy_x, y = alpha_value, fill = alpha_value)) +
  geom_tile() +  # Use geom_tile to create the bar
  scale_fill_gradient(low = "grey", high = "black", guide = "colourbar", name = "Alpha") +  # Grayscale from light to dark
  scale_y_continuous(name = 'Power', limits = c(0, 1), breaks = seq(0, 1, by = 0.25), expand = c(0, 0)) +  # Customize y-axis for alpha
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),  # Remove x-axis label
    axis.text.x = element_blank(),   # Remove x-axis text
    axis.ticks.x = element_blank(),  # Remove x-axis ticks
    legend.position = "none"         # Hide any automatic legends
  )

# Show the plot
alpha_legend_plot

makerobustheatmap <- function(dfx, climvar, stpsize){
  # subset data
  x <- format_ee_data(dfx, climvar)
  
  # subset data for stippeling
  stdata <- x %>%
    filter(power >= 0.8) %>%
    mutate(power_indicator = ifelse(power > 0.8, 'High power (> 0.8)', 'Low power (< 0.8)'))  # Adjust as necessary
  
  # create heatmap
  p <- ggplot() +
    # First, the geom_tile layer with fill for `metric`
    geom_tile(data = x, aes(x = intensity, y = duration, fill = metric, alpha = power)) +  # Removes borders around the tiles
    # scale_alpha_identity() +
    
    # Use scale_alpha_continuous for continuous alpha mapping
    scale_alpha_continuous(range = c(0.25, 1)) +  # Adjust the range if necessary
    
    # Apply the correct scale based on metric type
    scale_fill_viridis_d() +  # For continuous metric, or use scale_fill_viridis_d() for discrete
    
    # Second, the geom_point layer with shape for `power_indicator`
    geom_point(data = stdata, aes(x = intensity, y = duration, shape = power_indicator), 
               size = stpsize, color = 'black', alpha = 0.5) +  # Stippling effect
    
    theme_bw() +
    facet_grid(susceptibility ~ regime, scales = 'free') +
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
  
  # Arrange the plots with alpha_legend_plot being smaller
  combined_plot <- p + alpha_legend_plot + 
    plot_layout(ncol = 2, widths = c(4, 0.1))  # Adjust heights as needed
  
  savedir <- '../figures/heatmaps_regimes/'
  filePath <- paste0(savedir, climvar, '_robust', '.png')
  ggsave(combined_plot, file = filePath, dpi = 300, width = 10, height = 6.5) 
}

# create and save heat maps
makerobustheatmap(dfx = prain, climvar = 'rain', stpsize = 3)
makerobustheatmap(dfx = ptemp, climvar = 'temp', stpsize = 1)
