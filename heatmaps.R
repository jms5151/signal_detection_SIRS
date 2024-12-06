library(tidyverse)
library(stringr)
library(ggplot2)
library(patchwork)

# read in data
x = readRDS('../data/sim_summaries/long_summary.RData')

# factor levels
x$regime <- factor(x$regime, levels = c('Dry', 'Moderate', 'Wet', 'Temperate', 'Warm', 'Hot'))

# Define a normalization function that shifts and scales values between 0 and 1
normalize_shift <- function(x) {
  shifted <- x - min(x)  # Shift to make all values positive
  shifted / max(shifted)  # Normalize to [0, 1]
}

# Apply normalization to data and map to RGB channels
data <- x %>%
  mutate(
    # Normalize and shift each metric
    peak_timing_norm = normalize_shift(peak_timing_diff),
    cumulative_proportion_norm = normalize_shift(cumulative_proportion_diff),
    outbreak_duration_norm = normalize_shift(outbreak_duration_diff),

    # Map to RGB channels
    R = peak_timing_norm,  # Red channel
    G = cumulative_proportion_norm,  # Green channel
    B = outbreak_duration_norm,  # Blue channel
    
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
tick_marks <- list(
  bottom = data.frame(
    x = seq(0, 1, by = 0.2),  # Bottom axis (C)
    y = rep(0, 6),
    label = as.character(seq(0, 1, by = 0.2))
  ),
  left = data.frame(
    x = seq(0.5, 0, by = -0.1),  # Left axis (B)
    y = seq(0, sqrt(3) / 2, length.out = 6),
    label = as.character(seq(0, 1, length.out = 6))
  ),
  right = data.frame(
    x = seq(0.5, 1, by = 0.1),  # Right axis (A)
    y = seq(0, sqrt(3) / 2, length.out = 6),
    label = as.character(seq(0, 1, length.out = 6))
  )
)

# Plot
triPlot <- ggplot(triangle_data, aes(x = x, y = y)) +
  geom_tile(aes(fill = color), width = 0.01, height = 0.01) +
  scale_fill_identity() +
  coord_equal() +
  theme_void() +
  labs(title = 'Legend:\nTrivariate impact from extreme event') +
  theme(plot.title = element_text(hjust = 0.5)) +
  # Add axis labels
  annotate('text', x = -0.19, y = 0.03, label = 'Total cases\n(more)', color = 'green', size = 3.5, hjust = 0.2) +
  annotate('text', x = 1.19, y = 0.03, label = 'Peak timing\n(later)', color = 'red', size = 3.5, hjust = 0.8) +
  annotate('text', x = 0.5, y = sqrt(3) / 2 + 0.08, label = 'Outbreak duration\n(longer)', color = 'blue', size = 3.5, hjust = 0.5) +
  # Add tick marks for each axis
  geom_text(data = tick_marks$bottom, aes(x = x, y = y - 0.02, label = label), inherit.aes = FALSE, size = 3, col = 'green') +
  geom_text(data = tick_marks$left, aes(x = x - 0.54, y = y, label = label), inherit.aes = FALSE, size = 3, col = 'blue') +
  geom_text(data = tick_marks$right, aes(x = x + 0.54, y = y, label = label), inherit.aes = FALSE, size = 3, col = 'red') +
  # Add axis lines
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 0), inherit.aes = FALSE) +  # Bottom axis
  geom_segment(aes(x = 0, y = 0, xend = 0.5, yend = sqrt(3) / 2), inherit.aes = FALSE) +  # Left axis
  geom_segment(aes(x = 1, y = 0, xend = 0.5, yend = sqrt(3) / 2), inherit.aes = FALSE)    # Right axis

triPlot

# Plot using ggplot2

combined_plot <- (wbd_plt / vbd_plt) | (triPlot)
combined_plot
ggsave(filename = '../figures/heatmaps_regimes/fingerprint_heatmap_plot1.pdf', plot = combined_plot, width = 11, height = 6.5)

hS <- subset(data, suscept == 'S_max')
hS$type <- ifelse(grepl('Dry|Moderate|Wet', hS$regime), 'WBD', 'VBD')

ggplot(hS, aes(x = intensity, y = duration)) + 
  geom_tile(aes(fill = color), show.legend = FALSE, width = 1) +
  # scale_fill_identity() +
  facet_wrap(type ~ regime, scales = 'free') +
  # labs(x = xLabel, y = yLabel, title = titleLabel, fill = '') +
  theme_bw() +
  theme(
    legend.position = 'none',
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

combined_plot2 <- (wbd_plt / vbd_plt)
ggsave(filename = '../figures/heatmaps_regimes/fingerprint_heatmap_plot2.pdf', plot = combined_plot2, width = 11, height = 6.5)

