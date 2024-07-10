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

library(ggplot2)

# Define the x range
x_range <- seq(1, 20, length.out = 100)

# Create data frames for each function
df_linear <- data.frame(x = x_range, y = normalize_function(linear_function, x_range), func = 'Linear')
df_linear2 <- data.frame(x = x_range, y = normalize_function(linear_function, x_range, a = -0.1), func = 'Linear2')
df_quadratic <- data.frame(x = x_range, y = normalize_function(quadratic_function, x_range), func = 'Quadratic') # a > 1, U-shaped, a < 1 = concave down
df_quadratic2 <- data.frame(x = x_range, y = normalize_function(quadratic_function, x_range, a = -0.001, b = 0.021, c = 4), func = 'Quadratic2') # a > 1, U-shaped, a < 1 = concave down
df_exp_growth <- data.frame(x = x_range, y = normalize_function(exponential_growth_function, x_range), func = 'Exponential Growth')
df_exp_decay <- data.frame(x = x_range, y = normalize_function(exponential_decay_function, x_range), func = 'Exponential Decay')
df_modified_briere <- data.frame(x = x_range, y = normalize_function(modified_briere, x_range), func = 'Modified Briere')
df_michaelis_menten <- data.frame(x = x_range, y = normalize_function(michaelis_menten, x_range), func = 'Michaelis Menten')

# Combine all data frames
df_all <- rbind(df_linear, df_linear2, df_quadratic, df_quadratic2, df_exp_growth, df_exp_decay, df_modified_briere, df_michaelis_menten)

# Plot all functions
ggplot(df_all, aes(x = x, y = y, color = func)) +
  geom_line(linewidth = 1) +
  facet_wrap(~func) +
  labs(title = "Normalized Functions",
       x = "X",
       y = "Normalized Y",
       color = "Function") +
  theme_minimal()

# far <- function(p0, p1){ (p1 - p0)/p1 }
# rr <- function(p0, p1){ p1/p0 }

p1_far <- function(x, p0){ -p0/(x-1) } # k = p0

p1_rr <- function(x, p0){ x * p0 }

# 1. Given a threshold x-value, what is the y (P0)
calc_p0 <- function(x, func){
  y <- func(x)
  return(y)
}

testP0 <- calc_p0(x = 6, func = modified_briere)

# 2. Given y (P0), calculate y's for range of FAR and RR
far_range <- seq(0, 1, 0.01)
rr_range <- seq(1, 10, 0.1)

testFARp1s <- p1_far(x = far_range, p0 = testP0)
testRRp1s <- p1_rr(x = rr_range, p0 = testP0)

# 3. Determine x's that correspond to FAR and RR y's (p1s)
library(rootSolve)

# Generic function to backcalculate x given y
find_all_x_for_y <- function(f, y, interval = c(-10, 10), tol = .Machine$double.eps^0.25) {
  root_function <- function(x) f(x) - y
  
  # Use uniroot.all to find all roots
  roots <- uniroot.all(root_function, interval = interval, tol = tol)
  
  return(roots)
}

find_all_x_for_y(f = modified_briere, y = 0.05, interval = x_range)

# Apply to a vector of y-values
apply_find_all_x_for_ys <- function(f, ys, interval = c(-10, 10), tol = .Machine$double.eps^0.25) {
  sapply(ys, function(y) find_all_x_for_y(f, y, interval, tol), simplify = FALSE)
}

testbackXs <- apply_find_all_x_for_ys(f = modified_briere, y = testFARp1s, interval = x_range)

library(data.table)

df <- data.table::rbindlist(lapply(testbackXs, function(x) {
  if (all(is.na(x))) {
    data.frame(matrix(NA, ncol = max(sapply(output, length))))
  } else {
    data.frame(t(x))
  }
}), fill = TRUE)

plot(df$X1, far_range, type = 'l', xlim =  c(0,20))
lines(df$X2, far_range)