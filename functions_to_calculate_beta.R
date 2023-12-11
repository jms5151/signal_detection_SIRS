# not sure if this is useful anymore
calculate_beta <- function(params, betaFun) {
  betaFun(params = params)
}

beta_linear <- function(slope, intercept, x){
  slope * x + intercept
}

beta_s_curve <- function(max_val, slope, midpoint, x){
  max_val / (1 + exp(-slope * (x - midpoint)))
}

beta_saturating <- function(max_rate, x, km_value) {
  (max_rate * x) / (km_value + x) # michaelis menten equation
  }
  

beta_briere <- function(b0, T_min, T_max, x){
  b0 * x * (x - T_min) * (T_max - x)  # Brière model equation
}

## this is how we scale between two values
library(scales)
beta_low = 0.0005
beta_high = 3

T_values <- seq(0, 40, length.out = 100)  # Temperature range
b0_value <- 0.003  # Brière model parameter
b1_value <- 0.2  # Brière model parameter
T_min_value <- 5  # Lower temperature limit
T_max_value <- 35  # Upper temperature limit
data <- generate_briere_data(T_values, b0_value, b1_value, T_min_value, T_max_value)

data_rescaled <- rescale(x = data$Growth_Rate, to = c(beta_low, beta_high))
plot.ts(data_rescaled)

# ### ---------------------------------------------------------------------
# # Function to simulate data with a linear relationship
# generate_linear_data <- function(slope, intercept, num_points) {
#   # Create x values
#   x <- runif(num_points, 0, 100)  # Generating random x values
# 
#   # Calculate y values based on the linear equation (y = mx + c + error)
#   error <- rnorm(num_points, mean = 0, sd = 10)  # Adding random error
#   y <- slope * x + intercept + error
# 
#   # Return a data frame with x and y values
#   data.frame(x = x, y = y)
# }
# 
# # Plotting function
# plot_linear_data <- function(data, slope) {
#   plot(data$x, data$y, col = "blue", main = paste("Simulated Data (slope =", slope, ")"),
#        xlab = "X", ylab = "Y")
#   abline(lm(y ~ x, data), col = "red")  # Add a regression line
# }
# 
# # Simulate and plot data with different slopes
# set.seed(123)  # Setting seed for reproducibility
# 
# # Generate data with different slopes and plot each
# for (slope in c(1, 2, 3)) {
#   data <- generate_linear_data(slope, 5, 100)  # Change 100 to the desired number of points
#   plot_linear_data(data, slope)
# }
# 
# 
# 
# # Function to simulate data with an S-shaped relationship (logistic function)
# generate_s_shaped_data <- function(midpoint, slope, max_val, num_points) {
#   # Create x values
#   x <- seq(-10, 10, length.out = num_points)  # Generating x values
# 
#   # Calculate y values based on the logistic function
#   y <- max_val / (1 + exp(-slope * (x - midpoint)))
# 
#   # Return a data frame with x and y values
#   data.frame(x = x, y = y)
# }
# 
# # Plotting function
# plot_s_shaped_data <- function(data, midpoint, slope, max_val) {
#   plot(data$x, data$y, type = "l", col = "blue", main = paste("Simulated S-shaped Data"),
#        xlab = "X", ylab = "Y")
# }
# 
# # Simulate and plot data with an S-shaped relationship
# midpoint_values <- c(-2, 0, 2)  # Different midpoint values
# slope_value <- 1.5  # Adjust the slope if needed
# max_value <- 10  # Adjust the maximum value of the curve
# 
# # Plot data for different midpoint values
# for (midpoint in midpoint_values) {
#   data <- generate_s_shaped_data(midpoint, slope_value, max_value, 100)  # Change 100 to the desired number of points
#   plot_s_shaped_data(data, midpoint, slope_value, max_value)
# }
# 
# # Function to simulate data with a saturating relationship (Michaelis-Menten equation)
# generate_saturating_data <- function(max_rate, km, substrate_concentration) {
#   # Calculate reaction rates using Michaelis-Menten equation
#   reaction_rate <- (max_rate * substrate_concentration) / (km + substrate_concentration)
# 
#   # Return a data frame with substrate concentration and reaction rates
#   data.frame(Substrate_Concentration = substrate_concentration, Reaction_Rate = reaction_rate)
# }
# 
# # Plotting function
# plot_saturating_data <- function(data, max_rate, km) {
#   plot(data$Substrate_Concentration, data$Reaction_Rate, type = "l", col = "blue",
#        main = "Simulated Saturating Data",
#        xlab = "Substrate Concentration", ylab = "Reaction Rate")
# }
# 
# # Simulate and plot data with a saturating relationship
# max_rate_values <- c(10, 15, 20)  # Different maximum rate values
# km_value <- 5  # Set the Michaelis constant (Km)
# 
# # Plot data for different maximum rate values
# for (max_rate in max_rate_values) {
#   substrate_concentration <- seq(0, 30, length.out = 100)  # Vary substrate concentration
#   data <- generate_saturating_data(max_rate, km_value, substrate_concentration)
#   plot_saturating_data(data, max_rate, km_value)
# }
# 
# 
# # Function to simulate data with a Brière relationship
# generate_briere_data <- function(T, b0, b1, T_min, T_max) {
#   # Calculate the growth rate using the Brière model
#   growth_rate <- b0 * T * (T - T_min) * (T_max - T)  # Brière model equation
# 
#   # Apply constraints to growth rate based on temperature limits
#   growth_rate[T <= T_min | T >= T_max] <- 0
# 
#   # Return a data frame with temperature and growth rates
#   data.frame(Temperature = T, Growth_Rate = growth_rate)
# }
# 
# # Plotting function
# plot_briere_data <- function(data, b0, b1, T_min, T_max) {
#   plot(data$Temperature, data$Growth_Rate, type = "l", col = "blue",
#        main = "Simulated Brière Relationship",
#        xlab = "Temperature", ylab = "Growth Rate")
# }
# 
# # Simulate and plot data with a Brière relationship
# T_values <- seq(0, 40, length.out = 100)  # Temperature range
# b0_value <- 0.003  # Brière model parameter
# b1_value <- 0.2  # Brière model parameter
# T_min_value <- 5  # Lower temperature limit
# T_max_value <- 35  # Upper temperature limit
# 
# data <- generate_briere_data(T_values, b0_value, b1_value, T_min_value, T_max_value)
# plot_briere_data(data, b0_value, b1_value, T_min_value, T_max_value)
