# Load required libraries
library(deSolve)

# https://daphnia.ecology.uga.edu/drakelab/wp-content/uploads/2014/07/seasonality.pdf

# seasonal.sir.model <- function(t, y, params) {
#   with(
#     as.list(c(y, params)), # y = vector of state variables
#     {
#       beta <- calculate_beta(t, params, betaFun)
#       dS <- mu * (1 - S) - beta * S * I + (theta * S)
#       dI <- beta * S * I - (mu + gamma) * I
#       dR <- gamma * I - mu * R
#       dx <- c(dS, dI, dR)
#       list(dx)
#     }
#   )
# }


seasonal.sir.model <- function(t, y, params) {
  with(
    as.list(c(y, params)),
    {
      # this function identifies the beta closest to time point t
      beta <- beta_vals[which.min(abs(t - seq_along(beta_vals)))]
      dS <- -(beta * S * I) + (mu * (1 - S)) + (theta * R)
      dI <- beta * S * I - (mu + gamma) * I
      dR <- (gamma * I) - (theta * R)
      return(list(c(dS, dI, dR)))
    }
  )
}
