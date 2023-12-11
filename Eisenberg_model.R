library(deSolve)

# not running properly!
Eisenberg_model <- function(t, state, params) {
  with(
    as.list(c(state, params)),
    {
      dS <- -(betaW * S * W * rainfall[t]) - (beta1 * S * I) + (mu * S) 
      dI <- (beta1 * S * I) - (gamma * I)
      dW <- eta * (I - W) 
      dR <- (gamma * I)
      return(list(c(dS, dI, dW, dR)))
    }
  )
} 

# Eisenberg_beta <- function(bW, rainfall){
#   bW = 0.00128
#   return(W * bW * rainfall)
# }

# state variable starting values
s = 1-0.0069
i = 0.0069
w = 0.01
r = 0
xstart <- c(S = s, I = i, W = w, R = r)

# sampling times
years <- 1
times <- seq(from = 1, to = 365 * years, by = 1)


# Function to simulate daily rainfall ---
simulate_daily_rainfall <- function(num_days, wet_seasons = 1) {
  # Parameters for rainfall simulation
  avg_rainfall = 5  # Average daily rainfall (mm)
  wet_season_prob = 0.2  # Probability of a wet day during the dry season
  wet_season_multiplier = 2  # Multiplier for wet days during wet season
  
  # Initialize daily rainfall vector
  rainfall <- numeric(num_days)
  
  # Simulate wet season(s)
  for (season in 1:wet_seasons) {
    start_day <- sample(1:(num_days - 30), 1)  # Start wet season randomly
    
    # Simulate daily rainfall during wet season
    for (day in start_day:(start_day + 30)) {
      rainfall[day] <- rpois(1, avg_rainfall * wet_season_multiplier)
    }
  }
  
  # Simulate dry season
  for (day in 1:num_days) {
    if (rainfall[day] == 0 && runif(1) < wet_season_prob) {
      rainfall[day] <- rpois(1, avg_rainfall)
    }
  }
  
  return(rainfall)
}

# Set the number of days in the simulation
num_days <- length(times)

# Set the number of wet seasons (1 or 2)
num_wet_seasons <- 2

# Simulate daily rainfall
rainfall_data <- simulate_daily_rainfall(num_days, num_wet_seasons)

# Plot the simulated rainfall data
plot(rainfall_data, type = 'l', xlab = 'Day', ylab = 'Rainfall (mm)', main = 'Simulated Daily Rainfall')


# SIR parameter values ---
params <- list(
  gamma =  1 / 4 # recovery rate / inverse generation time
  , beta1 = 0.243
  , betaW = 0.00128
  , rainfall = rainfall_data
  , eta = 0.111
  # , k = 0.0000293
  , mu = 7*10e-04 #1/64 # Haiti
)

out <- as.data.frame(
  ode(
    xstart
    , times = times
    , Eisenberg_model
    , params
    # , rtol = 1e-12
    , hmax = 1 / 120
  )
)

plot.ts(out)
plot(out$S, out$I, type = 'l')

