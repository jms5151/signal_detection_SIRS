# Define functions that describe the relationship between climate and transmission 
# initial parameters are set to run with a given x, but all parameters can be overwritten
linear_function <- function(x, a = 0.4, b = 5) {
  a * x + b
}

quadratic_function <- function(x, a = 0.09, b = -1.92, c = 11) {
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
