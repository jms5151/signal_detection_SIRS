# Step 1: SIR Model with Time-Varying Beta Parameter

library(deSolve)

# Function for the SIR model with births and deaths
sir_model <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    # Calculate birth and death rates
    birth_rate <- 0.8  # Adjust as needed
    death_rate <- 0.01  # Adjust as needed
    
    # Differential equations
    dS <- (birth_rate * S) - (beta * S * I) - (death_rate * S)
    dI <- (beta * S * I) - (gamma * I) - (death_rate * I)
    dR <- (gamma * I) - (death_rate * R)
    
    # Return the result as a list
    return(list(c(dS, dI, dR)))
  })
}

# Set initial values and parameters
initial_state <- c(S = 990, I = 10, R = 0)  # Initial population: S = Susceptible, I = Infected, R = Recovered
parameters <- c(beta = 0.3, gamma = 0.1)   # Transmission rate and recovery rate

# Time vector
times <- seq(0, 200, by = 1)  # Simulation time

# Function to solve SIR model
solve_sir <- function(beta_values) {
  parameters["beta"] <- beta_values
  
  out <- ode(y = initial_state, times = times, func = sir_model, parms = parameters)
  return(out)
}

x <- solve_sir(beta_values = bv)
plot.ts(x[,3])

# Step 2: Function to Simulate Climate (Temperature)
# Function to simulate temperature throughout the year
simulate_temperature <- function() {
  # Your custom function to generate temperature data over time
  # This can include variation in mean, frequency, etc.
  # For demonstration, a simple sinusoidal pattern is used here
  temperature <- 10 + 5 * sin((2 * pi / 365) * times)
  return(temperature)
}

# Step 3: Functions Relating Beta to Climate
# Example: Linear relationship between beta and temperature
linear_beta <- function(temperature, slope) {
  return(slope * temperature)
}

# You can define other functions like saturating, briere, S-shaped, etc.


# Step 4: Step 4: Simulating Extreme Temperature Events
# Function to simulate extreme events affecting temperature and disease dynamics
simulate_extreme_temperature_event <- function(normal_temperature, extreme_temperature, slope) {
  # Simulating an extreme event by using extreme temperature values
  extreme_beta_values <- linear_beta(extreme_temperature, slope)
  normal_beta_values <- linear_beta(normal_temperature, slope)
  
  # Solve SIR model with normal and extreme temperature beta values
  extreme_event_output <- solve_sir(extreme_beta_values)
  normal_event_output <- solve_sir(normal_beta_values)
  
  return(list(normal = normal_event_output, extreme = extreme_event_output))
}

# Step 5: Storing and Plotting Results
# Generate normal and extreme temperature profiles
normal_temperature <- simulate_temperature()
extreme_temperature <- simulate_temperature()  # Update this with extreme temperature values

# Simulate disease dynamics under normal and extreme temperature conditions
temperature_slope <- 0.05  # Slope for the linear beta function
simulation_results <- simulate_extreme_temperature_event(normal_temperature, extreme_temperature, temperature_slope)

# Plotting the results
plot(times, simulation_results$normal[, "I"], type = "l", col = "blue", xlab = "Time", ylab = "Infected", main = "Disease Dynamics Under Normal and Extreme Temperatures")
lines(times, simulation_results$extreme[, "I"], col = "red")
legend("topright", legend = c("Normal Temperature", "Extreme Temperature"), col = c("blue", "red"), lty = 1)


# simulate heat wave
# Function to simulate extreme events affecting temperature and disease dynamics
simulate_heat_wave_event <- function(normal_temperature, heat_wave_duration, heat_wave_magnitude, slope) {
  # Simulating a heat wave event by modifying temperature data
  extreme_temperature <- normal_temperature  # Start with normal temperatures
  heat_wave_end <- min(heat_wave_duration, length(normal_temperature))
  
  # Simulating a heat wave of increased magnitude for a certain duration
  extreme_temperature[1:heat_wave_end] <- normal_temperature[1:heat_wave_end] + heat_wave_magnitude
  
  # Using the extreme temperature values to calculate beta values
  extreme_beta_values <- linear_beta(extreme_temperature, slope)
  normal_beta_values <- linear_beta(normal_temperature, slope)
  
  # Solve SIR model with normal and extreme temperature beta values
  extreme_event_output <- solve_sir(extreme_beta_values)
  normal_event_output <- solve_sir(normal_beta_values)
  
  return(list(normal = normal_event_output, extreme = extreme_event_output))
}

# Simulate a heat wave and compare disease dynamics under normal and heat wave conditions
heat_wave_duration <- 6  # Set the duration of the heat wave
heat_wave_magnitude <- 10  # Set the increase in temperature during the heat wave

temperature_slope <- 0.05  # Slope for the linear beta function
simulation_results <- simulate_heat_wave_event(normal_temperature, heat_wave_duration, heat_wave_magnitude, temperature_slope)

# Plotting the results
plot(times, simulation_results$normal[, "I"], type = "l", col = "blue", xlab = "Time", ylab = "Infected", main = "Disease Dynamics Under Normal and Heat Wave Conditions")
lines(times, simulation_results$extreme[, "I"], col = "red")
legend("topright", legend = c("Normal Temperature", "Heat Wave"), col = c("blue", "red"), lty = 1)

# stochastic weather generator
# Function to simulate stochastic temperature variations
simulate_stochastic_temperature <- function(initial_temp, n_steps, volatility) {
  temperature <- numeric(n_steps)
  temperature[1] <- initial_temp
  
  for (i in 2:n_steps) {
    temperature[i] <- temperature[i - 1] + rnorm(1, sd = volatility)
  }
  
  return(temperature)
}

# Simulating a stochastic heat wave event
simulate_stochastic_heat_wave <- function(normal_temperature, heat_wave_duration, volatility, slope) {
  # Simulating a heat wave event with stochastic temperature
  heat_wave_end <- min(heat_wave_duration, length(normal_temperature))
  extreme_temperature <- simulate_stochastic_temperature(normal_temperature, length(normal_temperature), volatility)
  
  # Using the extreme temperature values to calculate beta values
  extreme_beta_values <- linear_beta(extreme_temperature, slope)
  normal_beta_values <- linear_beta(normal_temperature, slope)
  
  # Solve SIR model with normal and extreme temperature beta values
  extreme_event_output <- solve_sir(extreme_beta_values)
  normal_event_output <- solve_sir(normal_beta_values)
  
  return(list(normal = normal_event_output, extreme = extreme_event_output))
}

x <- simulate_stochastic_heat_wave(normal_temperature, 6, 5, 2)
plot.ts(x)
