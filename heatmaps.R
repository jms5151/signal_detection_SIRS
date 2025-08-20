library(tidyverse)
library(patchwork)

# source functions
source('functions_for_heatmaps.R')

# prepare data ------------
# load data
x <- readRDS('../data/sim_summaries/long_summary.RData')

# set susceptibility level
suscept_level <- 'S_max'  

# set symbols for regimes
symbol_specs <- tibble::tibble(
  regime = c('Dry', 'Wet', 'Temperate', 'Warm'),
  intensity = c(80, 50, 7, 2),
  duration = c(5, 4, 5, 18),
  shape = c('circle', 'square', 'diamond', 'triangle')
)


# Transform and subset data
wbd_data <- transform_data(x, c('Dry', 'Moderate', 'Wet'), suscept_level)
vbd_data <- transform_data(x, c('Temperate', 'Warm', 'Hot'), suscept_level)

# Add symbols
wbd_data <- add_symbols(wbd_data, symbol_specs)
vbd_data <- add_symbols(vbd_data, symbol_specs)
transformed_data <- rbind(wbd_data, vbd_data)

# Heatmaps ------------
wbd_plt <- plt_regimes(wbd_data, 'Heavy rainfall intensity (mm above threshold)', 'Heavy rainfall duration (days)', 'Water-borne disease',  c('Dry', 'Moderate', 'Wet'))
vbd_plt <- plt_regimes(vbd_data, 'Heatwave intensity (degrees Celsius above threshold)', 'Heatwave duration (days)', 'Vector-borne disease', c('Temperate', 'Warm', 'Hot'))

# Triangle legend ------------
rgb_lookup <- transformed_data %>% distinct(R, G, B, color)

grid <- expand.grid(R = seq(0, 1, by = 0.01), G = seq(0, 1, by = 0.01)) %>%
  mutate(B = 1 - R - G) %>%
  filter(R >= 0 & G >= 0 & B >= 0) %>%
  mutate(x = R + G/2, y = sqrt(3)/2 * G)

get_nearest_color <- function(R, G, B, data_rgb) {
  diffs <- (data_rgb$R - R)^2 + (data_rgb$G - G)^2 + (data_rgb$B - B)^2
  rgb_lookup$color[which.min(diffs)]
}

grid$color <- purrr::pmap_chr(grid[, c('R', 'G', 'B')], get_nearest_color, data_rgb = rgb_lookup)

# Add symbols
symbol_rows <- transformed_data %>%
  filter(!is.na(shape)) %>%
  # inner_join(symbol_specs, by = c('regime', 'duration', 'intensity')) %>%
  distinct(R, G, B, shape)  # Only keep unique color/shape combos

symbol_coords <- purrr::pmap_dfr(
  list(symbol_rows$R, symbol_rows$G, symbol_rows$B),
  ~ get_nearest_xy(..1, ..2, ..3, grid)
)

symbol_points <- bind_cols(symbol_coords, symbol_rows)

# plot
triPlot <- ggplot(grid, aes(x = x, y = y)) +
  geom_tile(aes(fill = color), width = 0.01, height = 0.01) +
  scale_fill_identity() +
  coord_equal() +
  theme_void() +
  labs(title = 'Trivariate impact from extreme event (legend)') +
  theme(plot.title = element_text(hjust = 0.5)) +
  annotate('text', x = -0.20, y = 0.06, label = 'Total\ncases\n(more)', color = 'blue', size = 3.5, hjust = 0.2) +
  annotate('text', x = 1.22, y = 0.06, label = 'Outbreak\nduration\n(longer)', color = 'red', size = 3.5, hjust = 0.8) +
  annotate('text', x = 0.5, y = sqrt(3)/2 + 0.05, label = 'Peak timing (later)', color = 'green', size = 3.5, hjust = 0.5) +
  annotate('segment', x = 0, y = 0, xend = 1, yend = 0) +
  annotate('segment', x = 0, y = 0, xend = 0.5, yend = sqrt(3)/2) +
  annotate('segment', x = 1, y = 0, xend = 0.5, yend = sqrt(3)/2) +
  geom_point(data = symbol_points, aes(x = x, y = y, shape = shape), size = 3, color = 'white', show.legend = FALSE) +
  scale_shape_manual(values = c(circle = 1, square = 0, triangle = 2, diamond = 5))

# Combine heatmap and triangle legend
combined_plot <- (wbd_plt / vbd_plt) | triPlot
combined_plot

# save
ggsave(filename = '../figures/heatmaps_regimes/fingerprint_heatmap_plot_Smax.pdf', plot = combined_plot, width = 11, height = 6.5)
ggsave(filename = '../figures/heatmaps_regimes/fingerprint_heatmap_plot_Smax.png', dpi= 500, plot = combined_plot, width = 11, height = 6.5)

