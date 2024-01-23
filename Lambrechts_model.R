library(deSolve)

Lambrechts_model <- function(t, state, params) {
  
  # Calculate beta as a function of temperature-dependent entomological factors 
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
  
  # ODE
  with(
    as.list(c(state, params)),
    {
      beta <- Lambrechts_beta(climate_var[t])
      dS <- -beta * (S * I)/N + mu * (N - S) # births: mu*S and deaths mu*N
      dI <- beta * (S * I)/N - (gamma * I)
      dR <- (gamma * I)
      return(list(c(dS, dI, dR), beta = beta))
    }
  )
} 

# # sampling times
# Yrs <- 3
# times <- seq(from = 1, to = 365 * Yrs, by = 1)
# time1 <- 250
# time2 <- 700
# 
# source('functions_to_simulate_climate.R')
# source('simulate_climate.R')
# 
# # state variable starting values
# s = 0.039
# i = 0.001
# r = 0.06
# xstart <- c(S = s, I = i, R = r)
# 
# # SIR parameter values
# gamma = 1/15  # recovery rate / inverse generation time
# mu = 7 * 10e-04 # birth/death rate

# 
# params <- list(
#   gamma =  1 / 15 # recovery rate / inverse generation time
#   , b0 = beta0
#   , temperature = clim_small_trend
#   , N = 1#s + i + r
#   , mu = 7*10e-04#1/71 # bangladesh; 74 brazil #7*10e-04 https://sineadmorris.github.io/post/the-sir-model/
# )
# 
# out <- as.data.frame(
#   ode(
#     xstart
#     , times
#     , Lambrechts_model
#     , params
#     # , rtol = 1e-12
#     # , hmax = 1 / 120
#   )
# )
# 
# lines(out$I, col = 'darkred', lwd = 2)
# plot.ts(out$I, ylab = 'Infected')
 # plot.ts(out)
# plot(out$S, out$I, type = 'l')
# 
# Assuming x is a list of temperature values
# For example: x <- c(temperature_value1, temperature_value2, ...)

# Apply across list ----------------
# 
# params <- list(
#   gamma = gamma
#   , b0 = beta0
#   , N = 1
#   , mu = mu
# )
# 
# out_list <- lapply(TS_BRAZIL[1:5], function(temperature) {
#   params$temperature <- temperature
#   as.data.frame(
#     ode(
#       xstart
#       , times
#       , Lambrechts_model
#       , params
#       # , rtol = 1e-12
#       # , hmax = 1 / 120
#     )
#   )
# })

# run in parallel ------------------
# library(doParallel)
# library(foreach)
# 
# # Assuming x is a list of temperature values
# # For example: x <- c(temperature_value1, temperature_value2, ...)
# 
# params <- list(
#   gamma = gamma
#   , b0 = beta0
#   , N = 1
#   , mu = mu
# )
# 
# # Set up parallel processing with doParallel
# start_time <- Sys.time()
# 
# cl <- makeCluster(detectCores())
# registerDoParallel(cl)
# 
# # chose location
# temp = TS_BRAZIL
# temp = TS_BANGLADESH
# 
# sir_out <- foreach(temperature = temp, .packages = c("deSolve")) %dopar% {
#   params$temperature <- temperature
#   as.data.frame(
#     ode(
#       xstart,
#       times,
#       Lambrechts_model,
#       params
#     )
#   )
# }
# 
# stopCluster(cl)  # Stop the parallel processing cluster
# end_time <- Sys.time()
# end_time - start_time
# 
# # add list element names
# names(sir_out) <- names(temp)
# 
# # add Re 
# sir_out <- lapply(sir_out, function(x) {
#   cbind(x, Re = x$beta * x$S / gamma)
# })
# 
# # save
# saveRDS(sir_out, '../output/brazil_sir_out.RData')
# saveRDS(sir_out, '../output/bangladesh_sir_out.RData')
# 
# 
# ### Bangladesh
# # Set up parallel processing with doParallel
# start_time <- Sys.time()
# 
# cl <- makeCluster(detectCores())
# registerDoParallel(cl)
# 
# bangladesh_sir_out <- foreach(temperature = TS_BANGLADESH, .packages = c("deSolve")) %dopar% {
#   params$temperature <- temperature
#   as.data.frame(
#     ode(
#       xstart,
#       times,
#       Lambrechts_model,
#       params
#     )
#   )
# }
# 
# stopCluster(cl)  # Stop the parallel processing cluster
# end_time <- Sys.time()
# end_time - start_time
# 
# # add list element names
# names(bangladesh_sir_out) <- names(TS_BANGLADESH)
# 
# # add Re 
# bangladesh_sir_out <- lapply(bangladesh_sir_out, function(x) {
#   cbind(x, Re = x$beta * x$S / gamma)
# })
# 
# # save
# saveRDS(bangladesh_sir_out, '../output/bangladesh_sir_out.RData')
# 
