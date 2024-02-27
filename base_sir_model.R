library(deSolve)

sir_model <- function(t, state, params) {
  with(
    as.list(c(state, params)),
    {
      dS <- -(betaW[t] * S * W) - (beta[t] * S * I) + (mu * (I + R)) 
      dI <- (betaW[t] * S * W) + (beta[t] * S * I) - (gamma * I) - (mu * I)
      dW <- eta * (I - W) 
      dR <- (gamma * I) - (mu * R)
      return(list(c(dS, dI, dW, dR)))
    }
  )
} 

# sampling times
years <- 20
times <- seq(from = 1, to = 365 * years, by = 1)

# create climate cos wave
frequency <- 4*pi/length(times)
days <- seq(1, length(times), by = 1)
x = 20
y = 10
k <- (x + y)/2 + (x - y)/2 * cos(days * frequency)
plot.ts(k)

# source('functions_to_simulate_climate.R')
k_extreme <- generate_extreme_event(x = k, time1 = 10, time2 = 2000, magnitude_change = 200, duration = 15, timing = 'peak')
# k_extreme <- generate_extreme_event(x = k, time1 = 100, time2 = 300, magnitude_change = 400, duration = 55, timing = 'peak')

plot.ts(k_extreme[[1]], col = 'blue')
lines(k)

Lambrechts_beta <- function(climate_var){
  # median estimate from Reunion Island outbreaks for b0
  b0 = 1.14492
  # Squared version
  if (climate_var < 12.286 | climate_var > 32.461) {
    res = 0.0
  } else {
    res = 0.001044 * climate_var * (climate_var - 12.286) *
      sqrt(32.461 - climate_var);
  }
  
  return(b0 * (res * res))
}

Eisenberg_beta <- function(climate_var){
  bW = 0.00128
  return(bW * climate_var)
}

df <- readRDS('../data/ee_data/ee_wbd_Ethiopia.RData')
wbd_beta <- sapply(df$iter1_normal, function(x) Eisenberg_beta(x))
wbd_beta_extreme <- sapply(df$iter1_100mm_10d_peak, function(x) Eisenberg_beta(x))
plot.ts(wbd_beta_extreme, col = 'blue')
lines(wbd_beta)

vbd_beta <- sapply(k, function(x) Lambrechts_beta(x))
vbd_beta_extreme <- sapply(k_extreme[[1]], function(x) Lambrechts_beta(x))
wbd_beta <- sapply(k, function(x) Eisenberg_beta(x))
wbd_beta_extreme <- sapply(k_extreme[[1]], function(x) Eisenberg_beta(x))

plot.ts(vbd_beta)
plot.ts(wbd_beta)

# state variable starting values
# s = 0.039
# i = 0.001
# r = 0.06

s = 1-0.0069
i = 0.0069
w = 0.01
r = 0

xstart <- c(S = s, I = i, W = w, R = r)

# wbd_beta <- sapply(ethiopia$iter5_normal, function(x) Eisenberg_beta(x))
# wbd_beta_extreme <- sapply(ethiopia$iter5_200mm_13d_peak, function(x) Eisenberg_beta(x))
# plot.ts(wbd_beta_extreme, col = 'blue')
# lines(wbd_beta)

# SIR parameter values ---
params <- list(
  gamma =  1 / 4 # recovery rate / inverse generation time
  , beta = rep(0.243, length(wbd_beta))
  , betaW = wbd_beta
  , eta = 0.111
  , mu = 7*10e-04#1/(74*365)
)

params <- list(
  gamma =  1 / 15 # recovery rate / inverse generation time
  , beta = vbd_beta
  , betaW = rep(0, length(times))
  , eta = 0
  , mu = 7*10e-04#1/(74*365)
)

out <- as.data.frame(
  ode(
    xstart
    , times = times
    , sir_model
    , params
  )
)

plot.ts(out$I)
lines(out$I, col = 'blue')
lines(out$I, col = 'purple')
# range(out$bW)
# plot(out$S, out$I, type = 'l')
#

# Changing gamma changes magnitude
