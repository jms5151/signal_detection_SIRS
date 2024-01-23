library(deSolve)

# not running properly!
Eisenberg_model <- function(t, state, params) {
  with(
    as.list(c(state, params)),
    {
      dS <- -(betaW * S * W * climate_var[t]) - (beta1 * S * I) + (mu * S) 
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
# s = 1-0.0069
# i = 0.0069
# w = 0.01
# r = 0
# xstart <- c(S = s, I = i, W = w, R = r)

# # sampling times
# years <- 1
# times <- seq(from = 1, to = 365 * years, by = 1)
# 
# # SIR parameter values ---
# params <- list(
#   gamma =  1 / 4 # recovery rate / inverse generation time
#   , beta1 = 0.243
#   , betaW = 0.00128
#   , climate_var = rainfall_data
#   , eta = 0.111
#   # , k = 0.0000293
#   , mu = 7*10e-04 #1/64 # Haiti
# )
# 
# out <- as.data.frame(
#   ode(
#     xstart
#     , times = times
#     , Eisenberg_model
#     , params
#     # , rtol = 1e-12
#     , hmax = 1 / 120
#   )
# )
# 
# plot.ts(out)
# plot(out$S, out$I, type = 'l')

