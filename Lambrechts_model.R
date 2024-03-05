library(deSolve)

Lambrechts_model <- function(t, state, params) {
  with(
    as.list(c(state, params)),
    {
      dS <-  -(beta[t] * S * I) + (mu * (I + R)) 
      dI <- (beta[t] * S * I) - (gamma * I) - (mu * I)
      dR <- (gamma * I) - (mu * R)
      return(list(c(dS, dI, dR), beta = beta[t]))
    }
  )
} 

