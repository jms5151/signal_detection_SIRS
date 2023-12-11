x <- read.csv('./data/output_lamb_T15_R11.csv')
quantile(x$beta0)
beta0 <- median(x$beta0)

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

## power ---------------
library(ggplot2)
library(reshape2)
library(gridExtra)
library(ggpubr)
library(treemapify)
library(RColorBrewer)

dengue <- read.csv('data/dengue_R0_historical.csv')

dengue <- subset(dengue, R0 < 30) # remove extreme outlier

# climzone <- 'Temperate'
meanR0 <- round(mean(dengue$R0))
sdR0 <- sd(dengue$R0)
maxR0 <- round(max(dengue$R0))

r0_normal_95 <- function(N, gamma, beta, sd){
  (N / gamma) * beta + 1.645 * ((N * sd) / gamma)
}

percentExtreme <- function(N, gamma, r0_normal, beta_extreme, sd_extreme){
  pnorm(r0_normal
        , mean = (N / gamma) * beta_extreme
        , sd = (N / gamma) * sd_extreme
        , lower.tail = TRUE
        , log.p = FALSE)
}

N = 10000
N = 1 # cholera
gamma = 1/15 # dengue

gamma = 1/4 # cholera
meanR0 <- 2.15 # cholera
maxR0 <- 18 # cholera
mu = 0
beta_normal = (meanR0 / N) * (gamma + mu)

varyBetaMultiplier = seq(1, 4, by = 0.1)
vary_SD = beta_normal * varyBetaMultiplier
vary_R0_extreme = seq(meanR0, maxR0, 0.5) # Say that extreme weather events can at most double the R0

res = c()
for (R0_extreme_input in vary_R0_extreme) {
  for (sd_input in vary_SD) {
    R0_normal = meanR0
    R0_extreme = R0_extreme_input
    beta_normal = (R0_normal / N) * (gamma + mu)
    beta_extreme = (R0_extreme / N) * (gamma + mu)
    sd.error.normal = sd_input #beta_normal
    sd.error.extreme = sd_input
    ninetyfivepct.R0_normal = (N / (gamma + mu)) * beta_normal + 1.645 * ((N * sd.error.normal) / (gamma + mu))
    pct.extreme.dist.under.ninetyfivepct.R0_normal <- pnorm(ninetyfivepct.R0_normal, mean = (N / (gamma + mu)) * beta_extreme, sd = (N / (gamma + mu)) * sd.error.extreme, lower.tail = TRUE, log.p = FALSE)
    power = 1 - pct.extreme.dist.under.ninetyfivepct.R0_normal
    res = c(res, power)
  }
}
finalMat = matrix(res, ncol=length(vary_SD), nrow=length(vary_R0_extreme), byrow=T)
rownames(finalMat) <- vary_R0_extreme
colnames(finalMat) <- round((as.numeric(vary_SD) / beta_normal), 2)
finalMat = data.frame(finalMat, check.names = F)
finalMat$id = vary_R0_extreme #seq(1, length(vary_R0_extreme), by=1)
melted <- melt(finalMat, id.var='id')
colnames(melted) <- c('vary_R0_extreme', 'vary_SD', 'value')
myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
sc <- scale_fill_gradientn(colours = myPalette(100))
temp_plot <- ggplot(melted, aes(y = vary_R0_extreme, x = vary_SD, fill = value)) + geom_tile() +
  sc+  labs(fill="Power") +
  ylab('R0, extreme') + xlab('Beta variability (assuming same for normal and extreme)') + ggtitle('') +
  theme_classic() +
  ggtitle('WBD')
temp_plot




x1 <- read.csv('./data/output_lamb_T15_R11.csv')
beta0 <- median(x1$beta0)

x <- read.csv('data/climate.csv') # typically peak in September

  
city <- 'Sao Paulo, Brazil'
# city <- 'Dhaka City, Bangladesh'
cityMean  <- mean(x$meanT[x$City == city])
cityAmp <- (max(x$maxT[x$City == city]) - min(x$minT[x$City == city]))/2
# what we really want here is day to day variability
cityNoise <- sd(x$meanT[x$City == city])

betaNormal <- Lambrechts_beta(b0 = beta0, temperature = cityMean)
cityTempRange <- seq(cityRange[1], cityRange[2], by = 0.1)
betaNormalRange <- sapply(cityTempRange, Lambrechts_beta, b0 = beta0)
betaNormalSD <- sd(betaNormalRange)

n = 10000
mu = 1/71
gamma = 1/15

Dhaka_base_r0 <- r0_normal_95(N = n, mu = mu, gamma = gamma, beta = betaNormal, sd = 0.01)

MaxExtreme <- round(max(x$recordHighT[x$City == city])) #x$recordHighT[x$month == 8]
extremeTemps <- seq(round(cityMean) + 1, MaxExtreme, by = 1)

res <- c()
de <- c()
for(i in 1:length(extremeTemps)){
  Dhaka_extreme <- Lambrechts_beta(b0 = beta0, temperature = extremeTemps[i])
  de <- c(de, Dhaka_extreme)
  extremeR0prob <- percentExtreme(N = n
          , mu = mu
          , gamma = gamma
          , r0_normal = Dhaka_base_r0
          , beta_extreme = Dhaka_extreme
          , sd_extreme = betaNormalSD)
  power <- 1 - extremeR0prob
  res <- c(res, power)
}
res

plot(extremeTemps, res, pch = 16)
