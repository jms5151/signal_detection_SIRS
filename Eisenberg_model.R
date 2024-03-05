library(deSolve)

Eisenberg_model <- function(t, state, params) {
  with(
    as.list(c(state, params)),
    {
      # beta = betaW
      dS <- -(beta[t] * S * W) - (beta1 * S * I) + (mu * (I + R)) 
      dI <- (beta[t] * S * W) + (beta1 * S * I) - (gamma * I) - (mu * I)
      dW <- eta * (I - W) 
      dR <- (gamma * I) - (mu * R)
      return(list(c(dS, dI, dW, dR), beta = beta[t]))
    }
  )
} 

