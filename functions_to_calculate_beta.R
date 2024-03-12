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