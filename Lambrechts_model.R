library(deSolve)

Lambrechts_model <- function(t, state, params) {
  with(
    as.list(c(state, params)),
    {
      beta <- Lambrechts_beta(b0, temperature[t])
      dS <- -beta * (S * I)/N + mu * (N - S) # births: mu*S and deaths mu*N
      dI <- beta * (S * I)/N - (gamma * I)
      dR <- (gamma * I)
      return(list(c(dS, dI, dR), beta = beta))
    }
  )
} 

Lambrechts_beta <- function(b0, temperature){
  # Taken from: L. Lambrechts et al, PNAS 108 (2011)
  # Squared version
  if (temperature < 12.286 | temperature > 32.461) {
    res = 0.0
  } else {
    res = 0.001044 * temperature * (temperature - 12.286) *
      sqrt(32.461 - temperature);
  }
  
  return(b0 * (res * res))
}

x <- read.csv('./data/output_lamb_T15_R11.csv')
beta0 <- median(x$beta0)

# Lambrechts_beta(b0 = beta0, temperature = 21)

# sampling times
years <- 1
times <- seq(from = 1, to = 365 * years, by = 1)

# state variable starting values
s = 0.06#6000
i = 0.001#10
r = 0.04#93990
xstart <- c(S = s, I = i, R = r)

source('functions_to_simulate_climate.R')
source('simulate_climate.R')

# SIR parameter values
# params <- list(
#   gamma =  1 / 15 # recovery rate / inverse generation time
#   , b0 = beta0
#   , temperature = TS_BRAZIL[[1]]
#   , N = s + i + r
#   , mu = 7*10e-04#1/71 # bangladesh; 74 brazil #7*10e-04 https://sineadmorris.github.io/post/the-sir-model/
# )
# 
# out <- as.data.frame(
#   ode(
#     xstart
#     , times
#     , Lambrechts_model
#     , params <- 
#     # , rtol = 1e-12
#     # , hmax = 1 / 120
#   )
# )
# 
# lines(out$I, col = 'lightblue')
# plot.ts(out$I)
# plot.ts(out)
# plot(out$S, out$I, type = 'l')
# 
# Assuming x is a list of temperature values
# For example: x <- c(temperature_value1, temperature_value2, ...)

# Apply across list ----------------
# gamma = 1/15  # recovery rate / inverse generation time
# mu = 7 * 10e-04
# 
# params <- list(
#   gamma = gamma
#   , b0 = beta0
#   , N = s + i + r
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
library(doParallel)
library(foreach)

# Assuming x is a list of temperature values
# For example: x <- c(temperature_value1, temperature_value2, ...)

params <- list(
  gamma = gamma,
  b0 = beta0,
  N = s + i + r,
  mu = mu
)

# Set up parallel processing with doParallel
cl <- makeCluster(detectCores())
registerDoParallel(cl)

brazil_sir_out <- foreach(temperature = TS_BRAZIL, .packages = c("deSolve")) %dopar% {
  params$temperature <- temperature
  as.data.frame(
    ode(
      xstart,
      times,
      Lambrechts_model,
      params
    )
  )
}

stopCluster(cl)  # Stop the parallel processing cluster

# add list element names
names(brazil_sir_out) <- names(TS_BRAZIL)

# add Re 
brazil_sir_out <- lapply(brazil_sir_out, function(x) {
  cbind(x, Re = x$beta * x$S / gamma)
})

# save
saveRDS(brazil_sir_out, '../output/brazil_sir_out.RData')

# post-processing
groups <- gsub('iter[0-9]|100_', '', names(TS_BRAZIL))

brazil_betaSD <- lapply(brazil_sir_out, function(x){sd(x$beta)})
names(brazil_betaSD) <- names(TS_BRAZIL)
brazil_ReSD <- lapply(brazil_sir_out, function(x){sd(x$Re)})
names(brazil_ReSD) <- names(TS_BRAZIL)
brazil_finalSize <- lapply(brazil_sir_out, function(x){sum(x$I)})
names(brazil_ReSD) <- names(TS_BRAZIL)

norms <- names(TS_BRAZIL)[grepl('normal', names(TS_BRAZIL))]
m1 = mean(unlist(brazil_betaSD[norms]))
s1 = sd(unlist(brazil_betaSD[norms]))

EV25C15D <- names(TS_BRAZIL)[grepl('5C_3d_peak', names(TS_BRAZIL))]
m2 = mean(unlist(brazil_betaSD[EV25C15D]))
s2 = sd(unlist(brazil_betaSD[EV25C15D]))

# power anlaysis
install.packages("pwrss")
library(pwrss)

x <- pwrss.t.2means(mu1 = m1
               , mu2 = m2
               , sd1 = s1
               , sd2 = s2
               , kappa = 1 # kappa = sample size 1 / sample size 2
               , n2 = Iterations
               , alpha = 0.05
               , alternative = "not equal")
x$power
