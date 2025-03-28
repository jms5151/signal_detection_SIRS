# Plots prior extreme weather events versus outbreak status

# load libraries
library(ggplot2)
library(patchwork)
library(dplyr)
library(purrr)

# source('analyze_prior_ex_events.R) needed to be run first

# read in data
x <- read.csv('../data/extreme_events.csv')
x$Percentile[x$Extreme_climate_event == 'Drought'] <- 100 - x$Percentile[x$Extreme_climate_event == 'Drought']
x$Extreme_climate_event[x$Extreme_climate_event == 'Cyclone/Hurricane/Typhoon'] <- 'Cyclone/\nHurricane/Typhoon'

# average percentiles across studies
x2 <- x %>%
  filter(!is.na(Agreement)) %>%
  as.data.frame()

x2$Agreement <- factor(x2$Agreement, levels = c('High', 'Medium', 'Low'))

# Plotting extreme events: intensity vs group categories
# Plotting function
plotFun <- function(x, xName, colorName, custom_colors, colorLegendName, titleName){
  p <- ggplot(x, aes(x = xName, y = Percentile)) +
    geom_hline(yintercept = c(10, 90), linetype = 'dashed', color = 'grey') +
    geom_boxplot(color = "black", fill = NA) +
    geom_jitter(aes(fill = colorName), 
                width = 0.2, 
                size = 3, 
                shape = 21,     # allows fill + outline
                color = "black",  # black outline
                stroke = 0.5) +   # outline thickness
    scale_fill_manual(values = custom_colors) +  # apply your custom colors here
    theme_bw() +
    labs(title = titleName,
         x = '',
         y = '',
         fill = colorLegendName)  # use 'fill' for the legend title
  
  return(p)
}

# set custom colors
custom_colors_1 <- c('High' = '#d4ac0d', 'Medium' = 'purple', 'Low' = '#0c9e81')#, 'Insufficient sample size' = 'grey')

# plot
koppen_plot <- plotFun(x = x2, xName = x2$Koppen_Subgroup, colorName = x2$Agreement, custom_colors = custom_colors_1, colorLegendName = 'Agreement among studies\non climate-outbreak link', titleName = 'Köppen climate regime') +
  facet_grid( ~ Koppen_Group, scales = 'free_x', space = 'free')

disease_plot <- plotFun(x = x2, xName = x2$Disease, colorName = x2$Agreement, custom_colors = custom_colors_1, colorLegendName = 'Agreement among studies\non climate-outbreak link', titleName = 'Disease type') 

climate_plot <- plotFun(x = x2, xName = x2$Extreme_climate_event, colorName = x2$Agreement, custom_colors = custom_colors_1, colorLegendName = 'Agreement among studies\non climate-outbreak link', titleName = 'Climate event') 

# combine plots
y_axis_label <- ggplot() +
  geom_text(aes(0, 0, label = 'Extreme event (percentile)'), angle = 90, size = 4, hjust = 0.5, vjust = 0.5) +
  theme_void()  # Remove all background and axis elements

combined_plot <- (y_axis_label | koppen_plot / (disease_plot | climate_plot)) + 
  plot_layout(widths = c(0.07, 1), guides = 'collect') +
  plot_annotation('Extreme event driven outbreaks, cateogrized by:') &
  theme(legend.position = "bottom")

# Display the combined plot
combined_plot

# save
ggsave(filename = '../figures/csid_comparison.pdf', plot = combined_plot, width = 11, height = 6.5)

# Attributable fraction for cholera
source('far_and_rr_functions.R')

# Define functions
# Beta function
betafun <- function(b1, bW, rainfall) {
  b1 + (bW * rainfall)
}

# AF curve for one (b1, bW)
calc_AF_curve <- function(p0_rain, rain, b1, bW) {
  p0 <- betafun(b1, bW, p0_rain)
  p1s <- betafun(b1, bW, rain)
  FAR <- calc_FAR(p0, p1s)
  RR <- calc_RR(p0, p1s)
  calc_AF(FAR, RR)
}

# Deviation from threshold
calc_raindev <- function(rain, threshold) {
  rain / threshold
}

# Theory curves with uncertainty ribbons
make_theory_df_with_uncertainty <- function(quants, rain, bvals) {
  labels <- c("Dry", "Moderate", "Wet")
  
  purrr::map2_dfr(
    .x = quants,
    .y = labels,
    .f = function(q, label) {
      raindev <- calc_raindev(rain, q)
      
      af_mat <- purrr::map2_dfc(
        .x = bvals$b1,
        .y = bvals$bW,
        .f = function(b1, bW) {
          calc_AF_curve(q, rain, b1, bW)
        }
      )
      
      data.frame(
        raindev = raindev,
        AF_mean = rowMeans(af_mat),
        AF_lower = apply(af_mat, 1, quantile, probs = 0.025),
        AF_upper = apply(af_mat, 1, quantile, probs = 0.975),
        threshold = label
      )
    }
  )
}

# Observed AF with uncertainty per row
calc_observed_AF_uncertainty <- function(df, bvals) {
  af_list <- purrr::pmap(
    .l = list(b1 = bvals$b1, bW = bvals$bW),
    .f = function(b1, bW) {
      p0 <- betafun(b1, bW, df$Climate_threshold_80)
      p1 <- betafun(b1, bW, df$Climate_value)
      FAR <- calc_FAR(p0, p1)
      RR <- calc_RR(p0, p1)
      calc_AF(FAR, RR)
    }
  )
  
  af_mat <- do.call(cbind, af_list)
  
  df$AF_mean <- rowMeans(af_mat)
  df$AF_lower <- apply(af_mat, 1, quantile, probs = 0.025)
  df$AF_upper <- apply(af_mat, 1, quantile, probs = 0.975)
  
  return(df)
}

# Input data
bvals <- data.frame(
  b1 = c(0.212, 0.243, 0.155, 0.214),
  bW = c(0.00432, 0.00128, 0.00292, 0.00108)
)

# Subset cholera rows with non-missing rainfall
xrain <- subset(x, !is.na(Climate_value) & Disease == 'Cholera')

# Rain sequence
maxRain <- round(max(xrain$Climate_value))
rain <- seq(0, maxRain, 0.5)

# Rainfall threshold quantiles
xrain_quants <- quantile(xrain$Climate_threshold_80, c(0.1, 0.5, 0.9))

# Create theory data with uncertainty
af_theory_df <- make_theory_df_with_uncertainty(unname(xrain_quants), rain, bvals)

# Apply observational uncertainty
xrain <- xrain %>%
  mutate(
    raindev = calc_raindev(Climate_value, Climate_threshold_80),
    climate_regime = case_when(
      Climate_threshold_80 < quantile(Climate_threshold_80, 0.2) ~ 'Dry',
      Climate_threshold_80 > quantile(Climate_threshold_80, 0.8) ~ 'Wet',
      TRUE ~ 'Moderate'
    )
  )
xrain <- calc_observed_AF_uncertainty(xrain, bvals)

# Colors
threshold_cols <- c('Dry' = 'brown', 'Moderate' = 'lightblue', 'Wet' = 'blue')

# Plot
# ribbon in supplemental
af_obs_plot <- ggplot() +
  # Theoretical uncertainty ribbon
  # geom_ribbon(data = af_theory_df,
  #             aes(x = raindev, ymin = AF_lower, ymax = AF_upper, fill = threshold),
  #             alpha = 0.3) +
  # Theoretical mean line
  geom_line(data = af_theory_df,
            aes(x = raindev, y = AF_mean, color = threshold),
            linewidth = 1) +
  # Observational error bars
  geom_errorbar(data = xrain,
                aes(x = raindev, ymin = AF_lower, ymax = AF_upper, color = climate_regime),
                width = 0.1) +
  # Observational points
  geom_point(data = xrain,
             aes(x = raindev, y = AF_mean, fill = climate_regime),
             shape = 21, size = 3, stroke = 0.3, color = 'black') +
  # Colors
  scale_color_manual(values = threshold_cols) +
  scale_fill_manual(values = threshold_cols) +
  labs(x = 'Relative Ratio (total rainfall / exceedance threshold)',
       y = 'Attributable Fraction',
       fill = 'Climate regime',
       color = 'Climate regime') +
  theme_minimal(base_size = 13) +
  theme(legend.position = 'top') +
  theme_bw() +
  xlim(1,6) 

ggsave(filename = '../figures/af_obs_plot.pdf', plot = af_obs_plot, width = 8, height = 4)
ggsave(filename = '../figures/af_obs_plot_line_uncert.pdf', plot = af_obs_plot, width = 8, height = 4)
