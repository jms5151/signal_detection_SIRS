# source functions
source('seasonal_SIRS_model.R')
source('functions_to_simulate_climate.R')
source('functions_to_calculate_beta.R')
source('simulate_climate.R')


# 1. Generate climate time series with error
# 2. Generate related time series with extreme event and error
# 3. Calculate difference in sum(I) of outbreak
# 4. Repeat #1-3 500? times
# 5. Repeat #1-4 for different magnitude and duration 


# sampling times
times <- seq(from = 1, to = 365 * 20, by = 1)

# state variable starting values
xstart <- c(S = 6000, I = 10, R = 93990)

# SIR parameter values
immunity <- 1 / 365
recovery <- 1/7
mortality <- 1 / (70.5 * 365)

# find beta
R0 <- 2
b <- (R0 * mortality + R0 * recovery) / sum(xstart)
# find slope
climate_ts <- clim_small
bslope <- b / mean(climate_ts)

beta1 <- beta_linear(slope = bslope
                     , intercept = 0
                     , x = climate_ts
                     )

beta2 <- beta_linear(slope = bslope
                     , intercept = 0
                     , x = clim_small_trend
                     )

plot.ts(beta2)


# beta0=1000
# beta1=0.4
# t = seq(0,100, by = 1/120)
# beta <- beta0*(1+beta1*cos(2*pi*t))
# plot.ts(beta)

params <- list(
  mu = 1 / (70.5 * 365) # death rate
  , theta = 1 / 365 # waning immunity
  , gamma =  1 / 7 # recovery rate
  , beta_vals = beta1
)


# plot.ts(calculate_beta(times, params, betaFun = beta_saturating))

out <- as.data.frame(
  ode(
    xstart
    , times
    , seasonal.sir.model
    , params
    # , rtol = 1e-12
    # , hmax = 1 / 120
  )
)

plot.ts(out$I, lwd= 2, ylab = 'Infected')
lines(out$I, lwd= 2, col = 'orange')

# ----------------------------------------------------------------------

# Function to define the SIR model with a time-varying beta
time_varying_SIR <- function(time, state, parameters, beta_values) {
  with(as.list(c(state, parameters)), {
    # Get the current value of beta from the provided beta_values vector
    beta <- beta_values[time]
    
    # Differential equations
    dS <- -beta * S * I
    dI <- beta * S * I - gamma * I
    dR <- gamma * I
    
    # Return the derivatives
    return(list(c(dS, dI, dR), beta = beta))
  })
}

# Set the initial values and parameters (reduced initial infected)
initial_state <- c(S = 9999, I = 1, R = 0) # Initial values for S, I, R (reduced initial infected)
parameters <- c(gamma = 0.5) # Recovery rate

# Generate time points and define a less severe variation in beta over time
time_points <- 1:70
# we need to change this to create
# 1 a function that simulates climate through time, and
# 2 a function that relates to beta to climate (linear, nonlinear, s-shaped, saturating)
# and function 2 should have a sensitivity parameter that we can vary (i.e., a coefficient for slope or intercept)

# climate time series
climate_mean <- 20
climate_range <- 5
seasonality <- 3
frequency <- 50
climate <- climate_mean + sin(seasonality * pi * time_points / frequency) * climate_range 
plot.ts(climate)

# beta-climate relationship
intercept <- 1
slope <- 1
beta_climate_linear <- intercept + slope * climate

# beta time series
beta_values <- 0.0004 + sin(3 * pi * time_points / 50) * 0.1  # Less severe variation in beta over time
# beta_values <- rep(0.004, length(time_points))

# Simulate the SIR model with time-varying beta
sim_data <- as.data.frame(ode(y = initial_state, times = time_points, func = time_varying_SIR, parms = parameters, beta_values = beta_values))

# Visualize the simulated data
ggplot(sim_data, aes(x = time)) +
  geom_line(aes(y = S, color = "Susceptible")) +
  geom_line(aes(y = I, color = "Infected")) +
  geom_line(aes(y = R, color = "Recovered")) +
  labs(title = "SIR Model with Less Severe Outbreak (Time-Varying Beta)",
       x = "Time", y = "Population") +
  scale_color_manual(name = "Status",
                     values = c("Susceptible" = "blue", "Infected" = "red", "Recovered" = "green")) +
  theme_minimal() +
  xlim(0,50)

