library(ggplot2)
library(rootSolve)

# Define functions
# initial parameters are set to run with a given x, but all parameters can be overwritten
linear_function <- function(x, a = 0.1, b = 1) {
  a * x + b
}

quadratic_function <- function(x, a = 0.09, b = -1.92, c = 4) {
  a * x^2 + b * x + c
}

exponential_growth_function <- function(x, a = 0.1, b = 0.3) {
  a * exp(b * x)
}

exponential_decay_function <- function(x, a = 5, b = 0.3) {
  a * exp(-b * x)
}

modified_briere <- function(x, a = 0.0002, b = 4, c = 18) {
  ifelse(x > b & x < c, a * x * (x - b) * sqrt(c - x), 0)
}

michaelis_menten <- function(x, Vmax = 1, Km = 10) {
  Vmax * x / (Km + x)
}

normalize_function <- function(func, x_range, ...) {
  y_values <- func(x_range, ...)
  max_y <- max(y_values, na.rm = TRUE)
  min_y <- min(y_values, na.rm = TRUE)
  normalized_y <- (y_values - min_y) / (max_y - min_y) * 1.25
  return(normalized_y)
}

# Define the x range
x_range <- seq(1, 20, length.out = 100)

# Create data frames for each function
df_linear <- data.frame(x = x_range, y = normalize_function(linear_function, x_range), func = 'Linear')
df_linear2 <- data.frame(x = x_range, y = normalize_function(linear_function, x_range, a = -0.1), func = 'Linear2')
df_quadratic <- data.frame(x = x_range, y = normalize_function(quadratic_function, x_range), func = 'Quadratic') # a > 1, U-shaped, a < 1 = concave down
# df_quadratic2 <- data.frame(x = x_range, y = normalize_function(quadratic_function, x_range, a = -0.001, b = 0.021, c = 4), func = 'Quadratic2') # a > 1, U-shaped, a < 1 = concave down
df_exp_growth <- data.frame(x = x_range, y = normalize_function(exponential_growth_function, x_range), func = 'Exponential Growth')
df_exp_decay <- data.frame(x = x_range, y = normalize_function(exponential_decay_function, x_range), func = 'Exponential Decay')
df_modified_briere <- data.frame(x = x_range, y = normalize_function(modified_briere, x_range), func = 'Modified Briere')
df_michaelis_menten <- data.frame(x = x_range, y = normalize_function(michaelis_menten, x_range), func = 'Michaelis Menten')

# Combine all data frames
df_all <- rbind(df_linear, df_linear2, df_quadratic, df_exp_growth, df_exp_decay, df_modified_briere, df_michaelis_menten)

vline_data <- data.frame(
  func = rep(c('Exponential Decay', 'Exponential Growth', 'Linear', 'Linear2', 'Michaelis Menten', 'Modified Briere', 'Quadratic'), each = 3),
  xintercept = c(3, 5, 9, 8, 13, 18, rep(c(5, 9, 15), 2), 3, 7, 12, 6, 10, 16, 3, 7, 18),
  colType = rep(c('low', 'mod', 'high'), length(unique(df_all$func))),
  lineType = rep(c('solid', 'dashed', 'dotted'), length(unique(df_all$func)))
)

# detection and attribution
far_range <- seq(0, 1, 0.01)
rr_range <- seq(1, 10, 0.10)

p1_far <- function(x = far_range, p0){ -p0/(x-1) } # k = p0

p1_rr <- function(x = rr_range, p0){ x * p0 }

# 1. Given a threshold x-value, what is the y (P0)
calc_p0 <- function(x, func){
  y <- func(x)
  return(y)
}

# 2. Given y (P0), calculate y's for range of FAR and RR

# 3. Determine x's that correspond to FAR and RR y's (p1s)
# Generic function to backcalculate x given y
find_all_x_for_y <- function(f, y, interval = c(-10, 10), tol = .Machine$double.eps^0.25) {
  root_function <- function(x) f(x) - y
  
  # Use uniroot.all to find all roots
  roots <- uniroot.all(root_function, interval = interval, tol = tol)
  
  return(roots)
}

# Apply to a vector of y-values
apply_find_all_x_for_ys <- function(f, ys, interval = c(-20, 20), tol = .Machine$double.eps^0.25) {
  sapply(ys, function(y) find_all_x_for_y(f, y, interval, tol), simplify = FALSE)
}

create_ad_df <- function(xp0, trans_fun, ad_fun){
  # Calculate P0 for given x
  P0val <- calc_p0(x = xp0, func = trans_fun)
  # Calculate vector of FAR or RR
  p1vals <- ad_fun(p0 = P0val)
  # 3
  backcalcXs <- apply_find_all_x_for_ys(f = trans_fun, y = p1vals, interval = range(x_range))
  # create data frame of results
  first_elements <- sapply(backcalcXs, `[`, 1)
  second_elements <- sapply(backcalcXs, `[`, 2)
  df <- data.frame('Xp0' = xp0, 'X1' = first_elements, 'X2' = second_elements, 'p1' = p1vals)
  return(df)
}

Re_calc <- function(beta, S, gamma = 0.067){ # gamma = 0.25
  Re = (beta / gamma) * S
  Re[Re<0] <- 0
  Re[!is.finite(Re)] <- 0
  return(Re)
}

function_names = rep(c(exponential_decay_function, exponential_growth_function, linear_function, linear_function = function(x){linear_function(x = x, a = -0.1)}, michaelis_menten, modified_briere, quadratic_function), each = 3)

data <- data.frame()

for(i in 1:nrow(vline_data)){
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
    df <- create_ad_df(xp0 = vline_data$xintercept[i], trans_fun = function_names[[i]], ad_fun = p1_fun)
    # add id information
    df$variable <- p1name
    df$variable_value <- range_vals
    df$function_name <- vline_data$func[i]
    df$colType <- vline_data$colType[i]
    # add Re
    df$Low <- Re_calc(beta = df$p1, S = 0.2)
    df$High <- Re_calc(beta = df$p1, S = 0.8)
    data <- rbind(data, df)
  }
}

# format far and rr data
long_data <- data %>%
  pivot_longer(cols = c(X1, X2), names_to = 'root', values_to = 'root_value') %>%
  as.data.frame()

long_data$Xp0 <- as.character(long_data$Xp0)

# plotting functions
# value_colors <- c('low' = '#7A5A4F', 'mod' = '#f4c430', 'high' = 'red')
value_colors <- c('low' = 'darkorange4', 'mod' = 'lightblue', 'high' = 'blue')

plot_ad <- function(df, yLabel){
  p <- ggplot(df, aes(x = root_value, y = variable_value, group = interaction(root, colType), color = colType)) +
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

# format susceptibility data
long_data_s <- data %>%
  filter(variable == 'RR') %>%
  pivot_longer(cols = c(Low, High), names_to = 'Susceptibility', values_to = 'Re') %>%
  as.data.frame()

suscept_plot <- ggplot(long_data_s, aes(x = variable_value, y = Re, group = interaction(colType, Susceptibility), color = colType, linetype =  Susceptibility)) +
  geom_line(size = 1.1) +
  theme_light(base_size = 14) +
  facet_wrap(~function_name, nrow = 1) + 
  scale_color_manual(values = value_colors) + 
  guides(color = 'none') +
  xlab('Fold-change in transmission rate') +
  ylab('Re') +
  labs(color = NULL) +
  geom_hline(yintercept = 1, col = 'grey') +
  theme(legend.position = c(.92,.7)) +
  ylim(0,20) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )

# Plots
functions_plot <- ggplot(df_all, aes(x = x, y = y)) +
  geom_line(linewidth = 1) +
  facet_wrap(~func, nrow = 1) + # 
  labs(title = 'Transmission function',
       x = '',
       y = 'Normalized Y') +
  geom_vline(data = vline_data, aes(xintercept = xintercept, col = colType)) +
  # geom_vline(xintercept = c(5, 10, 15), linetype = 'dashed') +
  theme_light(base_size = 14) +
  scale_color_manual(values = value_colors) + 
  guides(color = 'none')


df_far = subset(long_data, variable == 'FAR')
far_plot <- plot_ad(df = df_far, yLabel = 'FAR')


df_rr = subset(long_data, variable == 'RR')
rr_plot <- plot_ad(df = df_rr, yLabel = 'RR') + xlab('X')

# plot together 
library(ggpubr)

multiplot_generic <- ggarrange(functions_plot, far_plot, rr_plot, ncol = 1)
ggsave(filename = '../figures/functional_forms/multiplot_generic.pdf', plot = multiplot_generic, width = 12, height = 7)

multiplot_generic_with_S <- ggarrange(functions_plot, far_plot, rr_plot, suscept_plot, ncol = 1)
ggsave(filename = '../figures/functional_forms/multiplot_generic_with_susceptibility.pdf', plot = multiplot_generic_with_S, width = 12, height = 7.5)
