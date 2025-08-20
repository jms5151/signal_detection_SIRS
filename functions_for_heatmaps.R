# transform data and color mapping
transform_data <- function(x, regimeTypes, suscept_level){
  x_new <- x %>%
  filter(regime %in% regimeTypes, suscept == !! suscept_level) %>% 
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
  
  return(x_new)
}

# plotting function
plt_regimes <- function(df, xLabel = '', yLabel = '', titleLabel = '', regime_order) {
  # Normalize and fix order once
  df <- df %>%
    dplyr::mutate(
      regime_clean = stringr::str_squish(as.character(regime)),
      regime_fac   = forcats::fct_relevel(regime_clean, regime_order)
    )
  
  ggplot(df, aes(x = intensity, y = duration)) + 
    geom_tile(aes(fill = color), show.legend = FALSE) +
    geom_point(
      data = dplyr::filter(df, !is.na(shape)),
      mapping = aes(shape = shape),
      size = 3, color = "white", show.legend = FALSE
    ) +
    scale_shape_manual(values = c(circle = 1, square = 0, triangle = 2, diamond = 5)) +
    scale_fill_identity() +
    # Use the cleaned, ordered factor for faceting
    facet_wrap(~ regime_fac, nrow = 1, scales = 'free', drop = FALSE) +
    labs(x = xLabel, y = yLabel, title = titleLabel) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
}



# function to add symbols
add_symbols <- function(data, symbol_specs) {
  left_join(data, symbol_specs, by = c('regime', 'duration', 'intensity'))
}

# function to get nearest coordinates for legend
get_nearest_xy <- function(r, g, b, triangle_df) {
  diffs <- (triangle_df$R - r)^2 + (triangle_df$G - g)^2 + (triangle_df$B - b)^2
  triangle_df[which.min(diffs), c('x', 'y')]
}