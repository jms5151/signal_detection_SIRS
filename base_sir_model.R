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

# sampling times
years <- 3
times <- seq(from = 1, to = 365 * years, by = 1)

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
lines(out$I, col = 'orange')