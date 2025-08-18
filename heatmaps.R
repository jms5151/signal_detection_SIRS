library(tidyverse)
library(patchwork)

# --- USER SETTINGS ---
suscept_level <- 'S_max'  # or 'S_min'

symbol_specs <- tibble::tibble(
  regime = c('Dry', 'Moderate', 'Hot', 'Warm'),
  duration = c(4, 7, 5, 18),
  intensity = c(20, 90, 6, 2),
  shape = c('circle', 'square', 'diamond', 'triangle')
)

# --- DATA TRANSFORMATION ---
x <- readRDS('../data/sim_summaries/long_summary.RData')
x$regime <- factor(x$regime, levels = c('Dry', 'Moderate', 'Wet', 'Temperate', 'Warm', 'Hot'))

# Transform and color mapping
transformed_data <- x %>%
  filter(suscept == suscept_level) %>%
  mutate(
    peak_timing_log = log(peak_timing_diff - min(peak_timing_diff) + 0.01),
    cumulative_prop_squared = (cumulative_proportion_diff - min(cumulative_proportion_diff) + 0.01)^2,
    outbreak_duration_sqrt = sqrt(outbreak_duration_diff - min(outbreak_duration_diff) + 0.01),
    
    peak_timing_z = scale(peak_timing_log)[, 1],
    cumulative_prop_z = scale(cumulative_prop_squared)[, 1],
    outbreak_duration_z = scale(outbreak_duration_sqrt)[, 1],
    
    peak_timing_pos = abs(peak_timing_z),
    cumulative_prop_pos = abs(cumulative_prop_z),
    outbreak_duration_pos = abs(outbreak_duration_z),
    
    total = peak_timing_pos + cumulative_prop_pos + outbreak_duration_pos + 1e-6,
    
    G = peak_timing_pos / total,
    B = cumulative_prop_pos / total,
    R = outbreak_duration_pos / total,
    color = rgb(R, G, B),
    
    R_rounded = round(R, 3),
    G_rounded = round(G, 3),
    B_rounded = round(B, 3)
  )

# --- PLOTTING FUNCTION ---
plt_regimes <- function(df, xLabel = '', yLabel = '', titleLabel = '') {
  ggplot(df, aes(x = intensity, y = duration)) + 
    geom_tile(aes(fill = color), show.legend = FALSE) +
    geom_point(data = df %>% filter(!is.na(shape)),
               mapping = aes(x = intensity, y = duration, shape = shape),
               size = 3, color = 'white', show.legend = FALSE) +
    scale_shape_manual(values = c(circle = 1, square = 0, triangle = 2, diamond = 5)) +
    scale_fill_identity() +
    facet_grid(~ regime, scales = 'free') +
    labs(x = xLabel, y = yLabel, title = titleLabel) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
}

# --- SUBSET DATA ---
wbd_data <- transformed_data %>%
  filter(regime %in% c('Dry', 'Moderate', 'Wet'), suscept == !! suscept_level)
vbd_data <- transformed_data %>%
  filter(regime %in% c('Temperate', 'Warm', 'Hot'), suscept == !! suscept_level)

# --- ADD SYMBOLS ---
add_symbols <- function(data, symbol_specs) {
  left_join(data, symbol_specs, by = c('regime', 'duration', 'intensity'))
}
wbd_data <- add_symbols(wbd_data, symbol_specs)
vbd_data <- add_symbols(vbd_data, symbol_specs)

# --- PLOT HEATMAPS ---
wbd_plt <- plt_regimes(wbd_data, 'Heavy rainfall intensity (mm above threshold)', 'Heavy rainfall duration (days)', 'Water-borne disease')
vbd_plt <- plt_regimes(vbd_data, 'Heatwave intensity (degrees Celsius above threshold)', 'Heatwave duration (days)', 'Vector-borne disease')

# --- TRIANGLE LEGEND ---
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

# --- SYMBOL POINTS IN LEGEND ---
symbol_rows <- transformed_data %>%
  filter(suscept == !!suscept_level) %>%
  inner_join(symbol_specs, by = c('regime', 'duration', 'intensity')) %>%
  distinct(R, G, B, shape)  # Only keep unique color/shape combos

get_nearest_xy <- function(r, g, b, triangle_df) {
  diffs <- (triangle_df$R - r)^2 + (triangle_df$G - g)^2 + (triangle_df$B - b)^2
  triangle_df[which.min(diffs), c('x', 'y')]
}

symbol_coords <- purrr::pmap_dfr(
  list(symbol_rows$R, symbol_rows$G, symbol_rows$B),
  ~ get_nearest_xy(..1, ..2, ..3, grid)
)

symbol_points <- bind_cols(symbol_coords, symbol_rows)

# --- FINAL TRIANGLE PLOT ---
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

# --- COMBINED PLOT ---
combined_plot <- (wbd_plt / vbd_plt) | triPlot
combined_plot

ggsave(filename = '../figures/heatmaps_regimes/fingerprint_heatmap_plot_Smax.pdf', plot = combined_plot, width = 11, height = 6.5)
ggsave(filename = '../figures/heatmaps_regimes/fingerprint_heatmap_plot_Smax.png', dpi= 500, plot = combined_plot, width = 11, height = 6.5)
# ggsave(filename = '../figures/heatmaps_regimes/fingerprint_heatmap_plot_Smin.pdf', plot = combined_plot, width = 11, height = 6.5)

