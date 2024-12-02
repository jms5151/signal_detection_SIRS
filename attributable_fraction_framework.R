# Libraries -----------------------------------------------------------
library(tidyverse)
library(patchwork)

# Load Functions ------------------------------------------------------
# Functions to describe the relationship between climate and transmission
source('transmission_functions.R')

# Functions to calculate FAR, RR, AF, and Re
source('far_and_rr_functions.R')

# Constants -----------------------------------------------------------
# Climate range
x_range <- seq(1, 20, by = 0.1)

# Susceptibility levels
s_low <- 0.2
s_high <- 0.8

# Colors for thresholds
value_colors <- c('farthest' = 'darkorange4', 'mid' = 'lightblue', 'closest' = 'blue')

# Create Climate-Beta Data --------------------------------------------
generate_beta_data <- function(func_name, x_range, func_label, ...) {
  data.frame(
    Climate = x_range,
    beta = normalize_function(func_name, x_range, ...),
    func = func_label
  )
}

# Data for each functional form
df_all <- bind_rows(
  generate_beta_data(linear_function, x_range, 'Linear (increasing)'),
  generate_beta_data(linear_function, x_range, 'Linear (decreasing)', a = -0.1),
  generate_beta_data(quadratic_function, x_range, 'Quadratic'),
  generate_beta_data(exponential_growth_function, x_range, 'Exponential Growth'),
  generate_beta_data(exponential_decay_function, x_range, 'Exponential Decay'),
  generate_beta_data(modified_briere, x_range, 'Modified Briere'),
  generate_beta_data(michaelis_menten, x_range, 'Michaelis Menten')
)

# Threshold Data ------------------------------------------------------
threshold_data <- tibble(
  func = rep(
    c('Exponential Decay', 'Exponential Growth', 'Linear (increasing)', 
      'Linear (decreasing)', 'Michaelis Menten', 'Modified Briere', 'Quadratic'), 
    each = 3
  ),
  threshold = c(3, 5, 9, 8, 13, 18, rep(c(5, 9, 15), 2), 3, 7, 12, 6, 10, 16, 3, 7, 18),
  threshold_level = rep(c('closest', 'mid', 'farthest'), 7)
)

# Calculate Attributable Fraction -------------------------------------
calculate_attribution <- function(df_all, threshold_data, s_low, s_high) {
  df <- tibble()
  
  for (i in seq_len(nrow(threshold_data))) {
    subset_data <- filter(df_all, func == threshold_data$func[i])
    thresholds <- unique(threshold_data$threshold[threshold_data$func == threshold_data$func[i]])
    
    for (th in thresholds) {
      temp_data <- subset_data
      temp_data$threshold <- th
      temp_data$threshold_level <- threshold_data$threshold_level[threshold_data$func == threshold_data$func[i] & threshold_data$threshold == th]
      beta_exceed <- temp_data$beta[temp_data$Climate == th]
      
      temp_data <- temp_data %>%
        mutate(
          FAR = calc_FAR(p0 = beta_exceed, p1 = beta),
          RR = calc_RR(p0 = beta_exceed, p1 = beta),
          AF = calc_AF(FAR = FAR, RR = RR),
          Low = Re_calc(beta = beta, S = s_low),
          High = Re_calc(beta = beta, S = s_high)
        )
      
      df <- bind_rows(df, temp_data)
    }
  }
  
  df <- df %>%
    mutate(
      climate_delta = Climate - threshold,
      threshold_level = factor(threshold_level, levels = c('farthest', 'mid', 'closest'))
    )
  
  list(
    full_data = df,
    long_data = df %>% 
      pivot_longer(cols = c(Low, High), names_to = 'Susceptibility', values_to = 'Re')
  )
}

attr_data <- calculate_attribution(df_all, threshold_data, s_low, s_high)
df <- attr_data$full_data
long_data_s <- attr_data$long_data

# Plotting Functions --------------------------------------------------
plot_functions <- function(df, thresholds) {
  ggplot(df, aes(x = Climate, y = beta)) +
    geom_vline(data = thresholds, aes(xintercept = threshold, col = threshold_level), size = 1.1) +
    geom_line(linewidth = 1) +
    facet_wrap(~func, nrow = 1) +
    labs(title = 'Transmission function', x = 'Climate variable', y = 'Transmission rate') +
    theme_bw(base_size = 14) +
    scale_color_manual(values = value_colors) +
    guides(color = 'none')
}

plot_attribution <- function(df) {
  ggplot(df, aes(x = climate_delta, y = AF, color = threshold_level)) +
    geom_line(linewidth = 1.1) +
    theme_bw(base_size = 14) +
    facet_wrap(~func, nrow = 1) +
    labs(
      title = 'Climate attribution', 
      x = 'Extreme event caused climate deviation', 
      y = 'Attributable fraction'
    ) +
    scale_color_manual(
      values = value_colors,
      name = 'Climate regime suitability',
      labels = c('closest' = 'High', 'mid' = 'Moderate', 'farthest' = 'Low')
    ) +
    theme(legend.position = 'bottom'
          , legend.background = element_rect(fill = 'transparent')
          , strip.background = element_blank()
          , strip.text.x = element_blank())
}

plot_susceptibility <- function(df) {
  ggplot(df, aes(x = climate_delta, y = Re, color = threshold_level, linetype = Susceptibility)) +
    geom_line(linewidth = 1.1) +
    geom_hline(yintercept = 1, size = 1, col = 'grey') +
    theme_bw(base_size = 14) +
    facet_wrap(~func, nrow = 1) +
    scale_color_manual(values = value_colors) +
    labs(
      title = 'Susceptibility impacts', 
      x = 'Extreme event caused climate deviation', 
      y = expression(R[e])
    ) +
    theme(legend.position = 'bottom'
          , legend.background = element_rect(fill = 'transparent')
          , strip.background = element_blank()
          , strip.text.x = element_blank())
}

# Combine and Save Plots ----------------------------------------------
create_multiplot <- function(df_all, thresholds, df_af, df_suscept) {
  p1 <- plot_functions(df_all, thresholds)
  p2 <- plot_attribution(df_af)
  p3 <- plot_susceptibility(df_suscept)
  
  (p1 / p2 / p3) +
    plot_layout(guides = "collect") & 
    theme(legend.position = "bottom")
}

mp_all <- create_multiplot(df_all, threshold_data, df, long_data_s)
ggsave('../figures/functional_forms/multiplot_all.pdf', plot = mp_all, width = 12, height = 7)

# Define the main functions to subset data
main_functions <- c('Exponential Growth', 'Linear (increasing)', 'Modified Briere', 'Quadratic')

# Subset data for selected functions
df_sub <- df_all %>% filter(func %in% main_functions)
df_af_sub <- df %>% filter(func %in% main_functions)
df_suscept_sub <- long_data_s %>% filter(func %in% main_functions)
td_sub <- threshold_data %>% filter(func %in% main_functions)

# Create and save the plot for main functions
mp_main <- create_multiplot(df_all = df_sub, thresholds = td_sub, df_af = df_af_sub, df_suscept = df_suscept_sub)
ggsave(filename = '../figures/functional_forms/multiplot_main.pdf', plot = mp_main, width = 8.5, height = 7)
