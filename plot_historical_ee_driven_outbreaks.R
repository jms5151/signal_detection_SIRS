# Plots prior extreme weather events versus outbreak status

# load libraries
library(ggplot2)
library(patchwork)

# source('analyze_prior_ex_events.R) needed to be run first

# read in data
x <- read.csv('../data/extreme_events.csv')
x$Percentile[x$Extreme_climate_event == 'Drought'] <- 100 - x$Percentile[x$Extreme_climate_event == 'Drought']
x$Extreme_climate_event[x$Extreme_climate_event == 'Cyclone/Hurricane/Typhoon'] <- 'Cyclone/\nHurricane/Typhoon'

# Plotting function
plotFun <- function(xName, colorName, custom_colors, colorLegendName, titleName){
  p <- ggplot(x, aes(x = xName, y = Percentile, color = colorName)) +
    geom_hline(yintercept = c(10, 90), linetype = 'dashed', color = 'grey') +
    geom_boxplot() +
    geom_jitter(width = 0.2, size = 3) +
    scale_color_manual(values = custom_colors) +
    theme_bw() +
    labs(title = titleName,
         x = '',
         y = '',
         color = colorLegendName)
  return(p)
}

# set custom colors
custom_colors_1 <- c('Mixed' = '#d4ac0d', 'No' = 'black', 'Yes' = '#0c9e81')

# plot
koppen_plot <- plotFun(xName = x$Koppen_Subgroup, colorName = x$Outbreak_risk, custom_colors = custom_colors_1, colorLegendName = 'Outbreak risk', titleName = 'Köppen climate regime') +
  facet_grid( ~ Koppen_Group, scales = 'free_x', space = 'free')

disease_plot <- plotFun(xName = x$Disease, colorName = x$Outbreak_risk, custom_colors = custom_colors_1, colorLegendName = 'Outbreak risk', titleName = 'Disease type') 

climate_plot <- plotFun(xName = x$Extreme_climate_event, colorName = x$Outbreak_risk, custom_colors = custom_colors_1, colorLegendName = 'Outbreak risk', titleName = 'Climate event') 

# combine plots
y_axis_label <- ggplot() +
  geom_text(aes(0, 0, label = 'Extreme event (percentile)'), angle = 90, size = 4, hjust = 0.5) +
  theme_void()  # Remove all background and axis elements

combined_plot <- (y_axis_label | koppen_plot / (disease_plot | climate_plot)) + 
  plot_layout(widths = c(0.07, 1), guides = 'collect') +
  plot_annotation('Extreme event driven outbreaks, cateogrized by:')

# Display the combined plot
combined_plot

# save
ggsave(filename = '../figures/csid_comparison.pdf', plot = combined_plot, width = 12, height = 6.5)

