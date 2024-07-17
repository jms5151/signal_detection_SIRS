# calculate p0 (i.e., y) given a threshold x-value and transmission function
calc_p0 <- function(x, func){
  y <- func(x)
  return(y)
}

# calculate p1 given x and fraction of attributable risk value(s)
p1_far <- function(x = far_range, p0){ -p0/(x-1) } # k = p0

# calculate p1 given x and relative risk value(s)
p1_rr <- function(x = rr_range, p0){ x * p0 }

# normalize y-data given an x range
normalize_function <- function(func, x_range, ...) {
  y_values <- func(x_range, ...)
  max_y <- max(y_values, na.rm = TRUE)
  min_y <- min(y_values, na.rm = TRUE)
  normalized_y <- (y_values - min_y) / (max_y - min_y) * 1.25
  return(normalized_y)
}

# back-calculate x given y (corresponding to FAR and RR ys (e.g., p1s)
find_all_x_for_y <- function(f, y, interval = c(-10, 10), tol = .Machine$double.eps^0.25) {
  root_function <- function(x) f(x) - y
  
  # Use uniroot.all to find all roots
  roots <- uniroot.all(root_function, interval = interval, tol = tol)
  
  return(roots)
}

# apply x back-calculation function above to a vector of y-values
apply_find_all_x_for_ys <- function(f, ys, interval = c(-20, 20), tol = .Machine$double.eps^0.25) {
  sapply(ys, function(y) find_all_x_for_y(f, y, interval, tol), simplify = FALSE)
}

create_ad_df <- function(xp0, trans_fun, ad_fun){
  # Calculate P0 for given x
  P0val <- calc_p0(x = xp0, func = trans_fun)
  # Calculate vector of FAR or RR
  p1vals <- ad_fun(p0 = P0val)
  # 3
  backcalcXs <- apply_find_all_x_for_ys(f = trans_fun, y = p1vals, interval = range(x_range))
  # create data frame of results
  first_elements <- sapply(backcalcXs, `[`, 1)
  second_elements <- sapply(backcalcXs, `[`, 2)
  df <- data.frame('Xp0' = xp0, 'X1' = first_elements, 'X2' = second_elements, 'p1' = p1vals)
  return(df)
}
