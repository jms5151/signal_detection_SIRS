# Parameter values
mu = 7 * 10e-04 # birth/death rate

# Lambrechts model
vbd.gamma = 1 / 15  # recovery rate / inverse generation time

# Eisenberg model
wbd.gamma =  1 / 4 # recovery rate / inverse generation time
wbd.beta1 = 0.243
wbd.eta = 0.111

# parameter lists
vbd.params <- list(
  gamma = vbd.gamma
  , mu = mu
)

wbd.params <- list(
  gamma =  wbd.gamma 
  , beta1 = wbd.beta1
  , eta = wbd.eta
  , mu = mu
)


