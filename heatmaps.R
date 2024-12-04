library(tidyverse)
library(stringr)
library(ggplot2)
library(patchwork)

# x = readRDS('../data/sim_summaries/highest_t_summary.RData')
# 
# x$regime <- factor(x$regime, levels = c('Temperate', 'Warm', 'Hot', 'Dry', 'Moderate', 'Wet'))
# 
# # Define the mapping for directions and corresponding colors
# direction_colors <- c(#1b9e77
#   "No difference" = 'black',
#   # time/duration change
#   "Shorter" = '#79530B',
#   "Earlier and shorter" = '#95680E',
#   "Later and shorter" = '#B17E10',
#   "Earlier" = '#CD9513',
#   "Earlier and longer" = '#E9AD16',
#   "Longer" = '#ECBA32',
#   "Later" = '#EFC64E',
#   "Later and longer" = '#F1D26A',
#   # larger outbreaks
#   "Larger" = '#9B0D46',
#   "Larger and longer" = '#AE0A3B',
#   "Later and larger" = '#C1062B',
#   "Earlier and larger" = '#D50116',
#   "Later, larger, and longer" = '#E60000',
#   "Earlier, larger, and longer" = '#F51400',
#   "Earlier, larger, and shorter" = '#FF3305',
#   # smaller outbreaks
#   "Smaller" = '#0EA0BE',
#   "Smaller and shorter" = '#1B6F8D',
#   "Later, smaller, and shorter" = '#214B63',
#   "Earlier, smaller, and shorter" = '#213140'#44c8b5
# )
# 
# # Recode `direction` and add a `color` column
# x <- x %>%
#   mutate(
#     direction = recode(
#       direction,
#       "same_PeakT_same_CumProp_less_Dur" = "Shorter",
#       "greater_PeakT_same_CumProp_less_Dur" = "Later and shorter",
#       "less_PeakT_same_CumProp_same_Dur" = "Earlier",
#       "same_PeakT_same_CumProp_same_Dur" = "No difference",
#       "greater_PeakT_greater_CumProp_greater_Dur" = "Later, larger, and longer",
#       "less_PeakT_greater_CumProp_greater_Dur" = "Earlier, larger, and longer",
#       "less_PeakT_same_CumProp_greater_Dur" = "Earlier and longer",
#       "less_PeakT_same_CumProp_less_Dur" = "Earlier and shorter",
#       "same_PeakT_less_CumProp_less_Dur" = "Smaller and shorter",
#       "same_PeakT_same_CumProp_greater_Dur" = "Longer",
#       "greater_PeakT_same_CumProp_same_Dur" = "Later",
#       "same_PeakT_greater_CumProp_greater_Dur" = "Larger and longer",
#       "greater_PeakT_same_CumProp_greater_Dur" = "Later and longer",
#       "less_PeakT_greater_CumProp_same_Dur" = "Earlier and larger",
#       "same_PeakT_greater_CumProp_same_Dur" = "Larger",
#       "greater_PeakT_less_CumProp_less_Dur" = "Later, smaller, and shorter",
#       "greater_PeakT_greater_CumProp_same_Dur" = "Later and larger",
#       "same_PeakT_less_CumProp_same_Dur" = "Smaller",
#       "less_PeakT_less_CumProp_less_Dur" = "Earlier, smaller, and shorter",
#       "less_PeakT_greater_CumProp_less_Dur" = "Earlier, larger, and shorter"
#     ),
#     color = direction_colors[direction] # Add color column based on direction
#   )
# 
# 
# 
# plt_regimes <- function(df, xLabel, yLabel, titleLabel){
#   p <- ggplot(df, aes(x = intensity, y = duration, fill = direction)) +
#     geom_tile() +
#     scale_fill_manual(values = direction_colors) +
#     facet_grid(~ regime, scales = 'free') +
#     labs(x = xLabel, y = yLabel, title = titleLabel, fill = '') +
#     theme_bw() +
#     theme(
#       legend.position = 'none',
#       panel.grid.major = element_blank(),
#       panel.grid.minor = element_blank()
#     )
#   return(p)
#   
# }
# 
# wdf = subset(x, regime == 'Dry'| regime == 'Moderate' | regime == 'Wet' )
# wdf = subset(wdf, suscept == 'S_max')
# wbd_plt <- plt_regimes(df = wdf, xLabel = 'Heavy rainfall intensity (mm above threshold)', yLabel = 'Heavy rainfall duration (days)', titleLabel = 'Water-borne disease')
# 
# vdf = subset(x, regime == 'Temperate'| regime == 'Warm' | regime == 'Hot')
# vdf = subset(vdf, suscept == 'S_max')
# vbd_plt <- plt_regimes(df = vdf, xLabel = 'Heatwave intensity (degrees Celsius above threshold)', yLabel = 'Heatwave duration (days)', titleLabel = 'Vector-borne disease')
# 
# # Custom legend data
# legend_data <- data.frame(
#   group = c('Outbreak type', rep('Change in timing/duration', 8), rep('Larger outbreaks', 7), rep('Smaller outbreaks', 4)),
#   category = names(direction_colors),
#   color = unname(direction_colors)
# )
# 
# legend_data$group <- factor(legend_data$group, levels = c('Outbreak type', 'Change in timing/duration', 'Larger outbreaks', 'Smaller outbreaks'))
# 
# # Create a standalone legend plot
# legend_plot <- ggplot(legend_data, aes(x = 1, y = category, color = color)) +
#   geom_point(size = 5, shape = 15) +
#   geom_text(aes(label = category), hjust = -0.2, size = 4) +
#   facet_wrap(~group, scales = "free_y", ncol = 4) +  # Group categories
#   scale_color_identity() +  # Use the exact colors provided in `color`
#   theme_void() +            # Remove axis lines and grids
#   theme(
#     strip.text = element_text(size = 12, face = "bold"),  # Group titles
#     panel.spacing = unit(0.0001, "lines"),                      # Reduce space between columns
#     legend.position = "none"                             # No default legend
#   )
# 
# 
# combined_plot <- (wbd_plt / vbd_plt) + 
#   legend_plot
# 
# ggsave(filename = '../figures/heatmaps_regimes/fingerprint_plot.pdf', plot = combined_plot, width = 11, height = 6.5)




# New Code -----------------------------
x = readRDS('../data/sim_summaries/?????.RData')

x$regime <- factor(x$regime, levels = c('Temperate', 'Warm', 'Hot', 'Dry', 'Moderate', 'Wet'))

# Plot using ggplot2

plt_regimes <- function(df, xLabel = '', yLabel = '', titleLabel = ''){
  p <- ggplot(df, aes(x = intensity, y = duration)) +
    geom_tile(aes(fill = color), show.legend = FALSE) +
    scale_fill_identity() +
    facet_grid(~ regime, scales = 'free') +
    labs(x = xLabel, y = yLabel, title = titleLabel, fill = '') +
    theme_bw() +
    theme(
      legend.position = 'none',
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
  return(p)
}

data1 <- subset(x, regime == 'Dry' | regime == 'Moderate' | regime == 'Wet')
data1 <- subset(data1, suscept == 'S_max')
wbd_plt <- plt_regimes(df = data1, xLabel = 'Heavy rainfall intensity (mm above threshold)', yLabel = 'Heavy rainfall duration (days)', titleLabel = 'Water-borne disease')

data1 <- subset(x, regime == 'Temperate' | regime == 'Warm' | regime == 'Hot')
data1 <- subset(data1, suscept == 'S_max')
vbd_plt <- plt_regimes(df = data1, xLabel = 'Heatwave intensity (degrees Celsius above threshold)', yLabel = 'Heatwave duration (days)', titleLabel = 'Vector-borne disease')

# Generate a dense triangular grid
n <- 200  # Higher value for smoother result
triangle_data <- expand.grid(A = seq(0, 1, length.out = n),
                             B = seq(0, 1, length.out = n)) %>%
  mutate(C = 1 - A - B) %>%
  filter(C >= 0) %>%  # Ensure points are inside the triangle
  mutate(R = A,  # Map A to Red
         G = B,  # Map B to Green
         B = C,  # Map C to Blue
         color = rgb(R, G, B))  # Combine into RGB color

# Convert to equilateral triangle coordinates
triangle_data <- triangle_data %>%
  mutate(x = A + B / 2,  # Adjust x for equilateral layout
         y = sqrt(3) / 2 * B)

# Plot using ggplot2
triPlot <-
ggplot(triangle_data, aes(x = x, y = y)) +
  geom_tile(aes(fill = color), width = 0.01, height = 0.01) +
  scale_fill_identity() +
  coord_equal() +
  theme_void() +
  labs(title = 'Legend:\nTrivariate impact from extreme event') +
  theme(plot.title = element_text(hjust = 0.5)) +
  # Add labels
  annotate('text', x = -0.19, y = 0.03, label = 'Total cases\n(more)', color = 'green', size = 3.5, hjust = 0.2) +
  annotate('text', x = 1.19, y = 0.03, label = 'Peak timing\n(later)', color = 'red', size = 3.5, hjust = 0.8) +
  annotate('text', x = 0.5, y = sqrt(3) / 2 + 0.08, label = 'Outbreak duration\n(longer)', color = 'blue', size = 3.5, hjust = 0.5)

combined_plot <- (wbd_plt / vbd_plt) | (triPlot)
# combined_plot

ggsave(filename = '../figures/heatmaps_regimes/fingerprint_heatmap_plot.pdf', plot = combined_plot, width = 11, height = 6.5)

