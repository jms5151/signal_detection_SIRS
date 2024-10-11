calc_FAR <- function(p0, p1){
  FAR = (p1 - p0) / p1
  FAR[FAR < 0] <- 0
  return(FAR)
}

calc_RR <- function(p0, p1){
  RR = p1 / p0
  RR[RR < 0] <- 0
  return(RR)
}

calc_AF <- function(FAR, RR){
  AF = FAR * (RR - 1) / RR
  AF[AF < 0] <- 0
  return(AF)
}

# calculate generic R-effective
Re_calc <- function(beta, S, gamma = 0.25){ 
  Re = (beta / gamma) * S
  Re[Re < 0] <- 0
  Re[!is.finite(Re)] <- 0
  return(Re)
}

# normalize y-data given an x range
normalize_function <- function(func, x_range, ...) {
  y_values <- func(x_range, ...)
  max_y <- max(y_values, na.rm = TRUE)
  min_y <- min(y_values, na.rm = TRUE)
  normalized_y <- (y_values - min_y) / (max_y - min_y) * 1.6
  return(normalized_y)
}