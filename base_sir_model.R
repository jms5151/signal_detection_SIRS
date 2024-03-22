library(deSolve)

sir_model <- function(t, state, params) {
  with(
    as.list(c(state, params)),
    {
      dS <- -(betaW[t] * S * W) - (beta[t] * S * I) + (mu * (I + R)) 
      dI <- (betaW[t] * S * W) + (beta[t] * S * I) - (gamma * I) - (mu * I)
      dW <- eta * (I - W) 
      dR <- (gamma * I) - (mu * R)
      return(list(c(dS, dI, dW, dR), betaW = betaW[t], beta = beta[t]))
    }
  )
} 

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


source('state_variables.R')
source('parameter_values.R')
source('functions_to_simulate_climate.R')

# sampling times
years <- 5
times <- seq(from = 1, to = 365 * years, by = 1)

x <- simulate_seasonal_climate(xmin = 0, xmax = 75, xvar = 0, seasons = 1, years = years)
wbd_beta <- sapply(x, function(x) Eisenberg_beta(x))
wbd_beta[wbd_beta<0] <- 0
plot.ts(wbd_beta)

exbeta <- generate_extreme_event(x, 0, 300, 100, 10, 'peak')
exbeta2 <- sapply(exbeta[[1]], function(x) Eisenberg_beta(x))
exbeta2[exbeta2<0] <- 0
plot.ts(exbeta2)

params <- list(
  gamma =  1 / 4 # recovery rate / inverse generation time
  , beta = rep(0.243, length(wbd_beta))
  , betaW = wbd_beta
  , eta = 0.111
  , mu = 7*10e-04#1/(74*365)
)

params <- list(
  gamma =  1 / 4 # recovery rate / inverse generation time
  , beta = rep(0.243, length(wbd_beta))
  , betaW = exbeta2
  , eta = 0.111
  , mu = 7*10e-04#1/(74*365)
)

# params <- list(
#   gamma =  1 / 15 # recovery rate / inverse generation time
#   , beta = vbd_beta
#   , betaW = rep(0, length(times))
#   , eta = 0
#   , mu = 7*10e-04#1/(74*365)
# )

out <- as.data.frame(
  ode(
    y = c(S = s, I = i, W = w, R = r)
    , times = times
    , sir_model
    , params
  )
)

plot.ts(out$I)

plot.ts(out2$I, col = 'lightblue')
lines(out$I)

plot.ts(out2$S)
lines(out$S, col  = 'orange')
hist(out$S)
