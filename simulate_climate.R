# simulate climate time series
source('ancillary_climate_data.R')
source('functions_to_simulate_climate.R')
source('time_spans.R')

sim_by_row <- function(df, clim = 'notrain') {
  result <- list()  # Initialize a list to store results
  for (i in 1:nrow(df)) {
    s <- simulate_seasonal_climate(
      xmin = df$Min_Month_Value[i],
      xmax = df$Max_Month_Value[i],
      xvar = 0,
      seasons = 1,
      years = Yrs
    )
    if(clim == 'rain'){
      s[s<0] <- 0
    }
    result[[i]] <- s  # Store the result for this row in the list
  }
  return(result)  # Return the list of results
}


addnames <- function(l, newname){
  names(l) <- paste(newname, seq(1:length(l)), sep = '_')
  return(l)
}

gen_clim_list <- function(df, clim = 'notrain', newname){
    x <- sim_by_row(df)
    x <- addnames(x, newname)
    return(x)
}

# generate climate time series
temperate_list <- gen_clim_list(df = t20, newname = 'normal_temperate')
warm_list <- gen_clim_list(df = t24, newname = 'normal_warm')
hot_list <- gen_clim_list(df = t28, newname = 'normal_hot')

dry_list <- gen_clim_list(df = dry, clim = 'rain', newname = 'normal_dry')
moderate_list <- gen_clim_list(df = mod, clim = 'rain', newname = 'normal_moderate')
wet_list <- gen_clim_list(df = wet, clim = 'rain', newname = 'normal_wet')

# save climate time series
savedir <- '../data/sim_climate/'
saveRDS(temperate_list, paste0(savedir, 'normal_temperate_clim.RData'))
saveRDS(warm_list, paste0(savedir, 'normal_warm_clim.RData'))
saveRDS(hot_list, paste0(savedir, 'normal_hot_clim.RData'))
saveRDS(dry_list, paste0(savedir, 'normal_dry_clim.RData'))
saveRDS(moderate_list, paste0(savedir, 'normal_moderate_clim.RData'))
saveRDS(wet_list, paste0(savedir, 'normal_wet_clim.RData'))