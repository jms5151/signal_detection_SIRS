library(tidyverse)
library(stringr)
library(ggplot2)
library(patchwork)

# read in data
x = readRDS('../data/sim_summaries/long_summary.RData')

# factor levels
x$regime <- factor(x$regime, levels = c('Dry', 'Moderate', 'Wet', 'Temperate', 'Warm', 'Hot'))

# tranform
x$peak_timing_shift <- x$peak_timing_diff - min(x$peak_timing_diff)
x$peak_timing_shift_log <- log(x$peak_timing_shift + 0.01)

x$cumulative_proportion_shift <- x$cumulative_proportion_diff - min(x$cumulative_proportion_diff)
x$cumulative_proportion_shift_squared <- (x$cumulative_proportion_shift + 0.01)^2

x$outbreak_duration_shift <- x$outbreak_duration_diff - min(x$outbreak_duration_diff)
x$outbreak_duration_shift_sqrt <- sqrt(x$outbreak_duration_shift + 0.01)

# Define a normalization function that shifts and scales values between 0 and 1
normalize_shift <- function(x) {
  shifted <- x - min(x)  # Shift to make all values positive
  shifted / max(shifted)  # Normalize to [0, 1]
}

# Apply normalization to data and map to RGB channels
data <- x %>%
  mutate(
    # Normalize and shift each metric
    peak_timing_norm = normalize_shift(peak_timing_shift_log),
    cumulative_proportion_norm = normalize_shift(cumulative_proportion_shift_squared),
    outbreak_duration_norm = normalize_shift(outbreak_duration_shift_sqrt),

    # Map to RGB channels
    G = peak_timing_norm,  # Green channel
    B = cumulative_proportion_norm,  # Blue channel
    R = outbreak_duration_norm,  # Red channel
    
    color = rgb(R, G, B, maxColorValue = 1)
    ) 

data$color[data$color == '#000000'] <- '#D3D3D3'

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


wbd_data <- subset(data, regime == 'Dry' | regime == 'Moderate' | regime == 'Wet')
wbd_data <- subset(wbd_data, suscept == 'S_max')
wbd_plt <- plt_regimes(df = wbd_data, xLabel = 'Heavy rainfall intensity (mm above threshold)', yLabel = 'Heavy rainfall duration (days)', titleLabel = 'Water-borne disease')

vbd_data <- subset(data, regime == 'Temperate' | regime == 'Warm' | regime == 'Hot')
vbd_data <- subset(vbd_data, suscept == 'S_max')
vbd_plt <- plt_regimes(df = vbd_data, xLabel = 'Heatwave intensity (degrees Celsius above threshold)', yLabel = 'Heatwave duration (days)', titleLabel = 'Vector-borne disease')

# legend
# Generate a complete grid of RGB combinations inside the triangle
color_data <- expand.grid(A = seq(0, 1, by = 0.01),  # Red channel
                          B = seq(0, 1, by = 0.01)) %>%  # Green channel
  mutate(C = 1 - A - B) %>%  # Blue channel to satisfy A + B + C = 1
  filter(C >= 0) %>%  # Keep points inside the triangle
  mutate(
    R = round(A, 3),  # Ensure values are rounded to 3 decimal places
    G = round(B, 3),
    B = round(C, 3),
    color = rgb(R, G, B)  # Generate hex color codes
  )

# Convert to color grid to equilateral triangle coordinates
triangle_data <- color_data %>%
  mutate(x = A + B / 2,  # Adjust x for equilateral layout
         y = sqrt(3) / 2 * B)

# Define tick marks for each axis as separate data frames
quants <- seq(0, 1, 0.25)  # Quantiles for the normalized scale

# Reversing normalization for each axis
tick_marks <- list(
  # peak timing
  bottom = data.frame(
    x = quants,  # Bottom axis (C)
    y = rep(0, length(quants)),
    label = as.character(rev(round(quants * (max(x$peak_timing_diff) - min(x$peak_timing_diff)) +
                                 min(x$peak_timing_diff), 2)*100))  # Reverse normalization
  ),
  # outbreak duration
  left = data.frame(
    x = seq(0.5, 0, length.out = length(quants)),  # Left axis (B)
    y = seq(0, sqrt(3) / 2, length.out = length(quants)),
    label = as.character(rev(round(quants * (max(x$outbreak_duration_diff) - min(x$outbreak_duration_diff)) +
                                     min(x$outbreak_duration_diff), 2)*100))  # Reverse normalization
  ),
  # cases
  right = data.frame(
    x = seq(0.5, 1, length.out = length(quants)),  # Right axis (A)
    y = seq(0, sqrt(3) / 2, length.out = length(quants)),
    label = as.character(round(quants * (max(x$cumulative_proportion_diff) - min(x$cumulative_proportion_diff)) +
                                     min(x$cumulative_proportion_diff), 2)*100)  # Reverse normalization
  )
)


# Plot
triPlot <- ggplot(triangle_data, aes(x = x, y = y)) +
  geom_tile(aes(fill = color), width = 0.01, height = 0.01) +
  scale_fill_identity() +
  coord_equal() +
  theme_void() +
  labs(title = 'Trivariate impact from extreme event\n(legend, % change)') +
  theme(plot.title = element_text(hjust = 0.5)) +
  # Add axis labels
  annotate('text', x = -0.20, y = 0.03, label = 'Peak\ntiming\n(later)', color = 'green', size = 3.5, hjust = 0.2) +
  annotate('text', x = 1.22, y = 0.03, label = 'Outbreak\nduration\n(longer)', color = 'red', size = 3.5, hjust = 0.8) +
  annotate('text', x = 0.5, y = sqrt(3) / 2 + 0.08, label = 'Total cases\n(more)', color = 'blue', size = 3.5, hjust = 0.5) +
  # Add tick marks for each axis
  geom_text(data = tick_marks$bottom, aes(x = x, y = y - 0.02, label = label), inherit.aes = FALSE, size = 3, col = 'green') +
  geom_text(data = tick_marks$left, aes(x = x + 0.54, y = y, label = label), inherit.aes = FALSE, size = 3, col = 'red') +
  geom_text(data = tick_marks$right, aes(x = x - 0.54, y = y, label = label), inherit.aes = FALSE, size = 3, col = 'blue') +
  # Add axis lines
  annotate("segment", x = 0, y = 0, xend = 1, yend = 0) +  # Bottom axis
  annotate("segment", x = 0, y = 0, xend = 0.5, yend = sqrt(3) / 2) +  # Left axis
  annotate("segment", x = 1, y = 0, xend = 0.5, yend = sqrt(3) / 2)   # Right axis

# triPlot

# Plot using ggplot2
combined_plot <- (wbd_plt / vbd_plt) | (triPlot)
combined_plot
ggsave(filename = '../figures/heatmaps_regimes/fingerprint_heatmap_plot1.pdf', plot = combined_plot, width = 11, height = 6.5)
ggsave(filename = '../figures/heatmaps_regimes/fingerprint_heatmap_plot_Smin.pdf', plot = combined_plot, width = 11, height = 6.5)
