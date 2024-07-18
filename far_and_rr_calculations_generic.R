# load libraries -----------------------------
library(ggplot2)
library(rootSolve)
library(tidyverse)
library(ggpubr)

# functions ---------------------------------
# source functions that describe the relationship between climate and transmission
source('transmission_functions.R')

# source functions for calculating attribution and detection metrics
source('far_and_rr_functions.R')

# calculate generic R-effective
Re_calc <- function(beta, S, gamma = 0.25){ 
  Re = (beta / gamma) * S
  Re[Re < 0] <- 0
  Re[!is.finite(Re)] <- 0
  return(Re)
}

# plot function
plot_ad <- function(df, yLabel){
  p <- ggplot(df, aes(x = root_value, y = variable_value, group = interaction(root, threshold_level), color = threshold_level)) +
    geom_line(size = 1.1) +
    theme_light(base_size = 14) +
    facet_wrap(~function_name, nrow = 1) + 
    scale_color_manual(values = value_colors) + 
    guides(color = 'none') +
    xlab('') +
    ylab(yLabel) +
    theme(
      strip.background = element_blank(),
      strip.text.x = element_blank()
    )
  return(p)
}

# Define values for calculations/visualizations ----------
# Define the x range
x_range <- seq(1, 20, length.out = 100)

# Define range of fraction of attributable risk
far_range <- seq(0, 1, 0.01)

# Define range of relative risk 
rr_range <- seq(1, 10, 0.10)

# Define susceptibility levels
s_low = 0.2
s_high = 0.8

# Create xy data for each functional form ---------------
df_linear <- data.frame(x = x_range, y = normalize_function(linear_function, x_range), func = 'Linear')
df_linear2 <- data.frame(x = x_range, y = normalize_function(linear_function, x_range, a = -0.1), func = 'Linear2')
df_quadratic <- data.frame(x = x_range, y = normalize_function(quadratic_function, x_range), func = 'Quadratic') # a > 1, U-shaped, a < 1 = concave down
df_exp_growth <- data.frame(x = x_range, y = normalize_function(exponential_growth_function, x_range), func = 'Exponential Growth')
df_exp_decay <- data.frame(x = x_range, y = normalize_function(exponential_decay_function, x_range), func = 'Exponential Decay')
df_modified_briere <- data.frame(x = x_range, y = normalize_function(modified_briere, x_range), func = 'Modified Briere')
df_michaelis_menten <- data.frame(x = x_range, y = normalize_function(michaelis_menten, x_range), func = 'Michaelis Menten')

# combine
df_all <- rbind(df_linear, df_linear2, df_quadratic, df_exp_growth, df_exp_decay, df_modified_briere, df_michaelis_menten)

# add functional form and threshold information
threshold_data <- data.frame(
  func = rep(c('Exponential Decay', 'Exponential Growth', 'Linear', 'Linear2', 'Michaelis Menten', 'Modified Briere', 'Quadratic'), each = 3),
  threshold = c(3, 5, 9, 8, 13, 18, rep(c(5, 9, 15), 2), 3, 7, 12, 6, 10, 16, 3, 7, 18),
  threshold_level = rep(c('low', 'mod', 'high'), length(unique(df_all$func)))
)

# calculate FAR and RR for different functional relationships and climate thresholds ---------
function_names = rep(c(exponential_decay_function, exponential_growth_function, linear_function, linear_function = function(x){linear_function(x = x, a = -0.2)}, michaelis_menten, modified_briere, quadratic_function), each = 3)

data <- data.frame()

for(i in 1:nrow(threshold_data)){
  for(j in 1:2){
    if(j == 1){
      p1_fun = p1_far
      p1name = 'FAR'
      range_vals = far_range
    } else {
      p1_fun = p1_rr
      p1name = 'RR'
      range_vals = rr_range
    }
    df <- create_ad_df(xp0 = threshold_data$threshold[i], trans_fun = function_names[[i]], ad_fun = p1_fun)
    # add id information
    df$variable <- p1name
    df$variable_value <- range_vals
    df$function_name <- threshold_data$func[i]
    df$threshold_level <- threshold_data$threshold_level[i]
    # add Re
    normalized_ys <- normalize_y(func = function_names[[i]], y_values = df$p1, min_y = min(function_names[[i]](x_range)), max_y = max(function_names[[i]](x_range)))
    df$Low <- Re_calc(beta = normalized_ys, S = s_low)
    df$High <- Re_calc(beta = normalized_ys, S = s_high)
    data <- rbind(data, df)
  }
}

# format far and rr data
long_data <- data %>%
  pivot_longer(cols = c(X1, X2), names_to = 'root', values_to = 'root_value') %>%
  as.data.frame()

long_data$Xp0 <- as.character(long_data$Xp0)

df_far = subset(long_data, variable == 'FAR')

df_rr = subset(long_data, variable == 'RR')

# format susceptibility data
long_data_s <- data %>%
  filter(variable == 'RR') %>%
  pivot_longer(cols = c(Low, High), names_to = 'Susceptibility', values_to = 'Re') %>%
  as.data.frame()

# Plot -------------------------------------------
# set colors for thresholds
value_colors <- c('low' = 'darkorange4', 'mod' = 'lightblue', 'high' = 'blue')

# generic functions plot
functions_plot <- ggplot(df_all, aes(x = x, y = y)) +
  geom_vline(data = threshold_data, aes(xintercept = threshold, col = threshold_level), size = 1.1) +
  geom_line(linewidth = 1) +
  facet_wrap(~func, nrow = 1) + # 
  labs(title = 'Transmission function',
       x = '',
       y = 'Transmission rate') +
  # geom_vline(xintercept = c(5, 10, 15), linetype = 'dashed') +
  theme_light(base_size = 14) +
  scale_color_manual(values = value_colors) + 
  guides(color = 'none')

# FAR plot
far_plot <- plot_ad(df = df_far, yLabel = 'FAR')

# RR plot
rr_plot <- plot_ad(df = df_rr, yLabel = 'RR') + xlab('Climate variable')

# susceptibility plot
suscept_plot <- ggplot(long_data_s, aes(x = variable_value, y = Re, group = interaction(threshold_level, Susceptibility), color = threshold_level, linetype =  Susceptibility)) +
  geom_line(size = 1.1) +
  theme_light(base_size = 14) +
  facet_wrap(~function_name, nrow = 1) + 
  scale_color_manual(values = value_colors) + 
  guides(color = 'none') +
  xlab('Fold-change in transmission rate') +
  ylab('Re') +
  labs(color = NULL) +
  geom_hline(yintercept = 1, col = 'grey') +
  theme(legend.position = c(.055,.68), legend.background = element_rect(fill='transparent')) +
  # ylim(0,10) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )

# arrange plots and save 
multiplot_generic <- ggarrange(functions_plot, far_plot, rr_plot, ncol = 1)
ggsave(filename = '../figures/functional_forms/multiplot_generic.pdf', plot = multiplot_generic, width = 12, height = 7)

multiplot_generic_with_S <- ggarrange(functions_plot, far_plot, rr_plot, suscept_plot, ncol = 1)
ggsave(filename = '../figures/functional_forms/multiplot_generic_with_susceptibility.pdf', plot = multiplot_generic_with_S, width = 12, height = 8.5)
