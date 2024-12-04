# Identify times of low and high population susceptibility for extreme event perturbations
library(stringr)

# list filepaths
sim_data_path <- '../data/sim_results/'
dis_files <- list.files(sim_data_path, full.names = T)
clim_data_path <- '../data/sim_climate/'
clim_files <- list.files(clim_data_path, full.names = T)

# function to determine min/max susceptibility and timing
Sid <- function(d, t1, t2){
  x2 <- lapply(d, function(x) list('S_min_t' = which(x$S == min(x$S[t1:t2])), 'S_max_t' = which(x$S == max(x$S[t1:t2]))))
  return(x2)
}

# function to plot climate, susceptible, and infected time series with min/max susceptibility points
basicPlot <- function(df, varName, varLabel, sList){
  plot(df$time, df[,varName], type = 'l', las = 1, ylab = varLabel, xlab = '')
  abline(v = sList[[1]]$S_max_t)
  abline(v = sList[[1]]$S_min_t, lty = 2)
}

# run loop to extract min/max susceptibility values and timing, save output, plot for verification 
for(i in 1:length(dis_files)){
  d <- readRDS(dis_files[i])
  regimeName <- gsub('../data/sim_results/normal_|.RData', '', dis_files[i])
  t1 = 300
  t2 = 600
  d2 <- Sid(d, t1, t2)
  newfilepath <- gsub('sim_results/', 'sim_sids/sid_', dis_files[i])
  saveRDS(d2, file = newfilepath)
  # plot
  c <- readRDS(clim_files[i])
  df <- as.data.frame(d) 
  df$climate <- c
  pdf(paste0('../figures/time_series/', regimeName, '.pdf'), width = 6, height = 6.5)
  par(mfrow = c(3,1), mai = c(0.5, 0.5, 0.1, 0.1), oma=c(0.5,0.5,1.5,0.5))
  basicPlot(df = df, varName = 'climate', varLabel = 'Climate', sList = d2)
  basicPlot(df = df, varName = 'S', varLabel = 'Susceptible (proportion)', sList = d2)
  basicPlot(df = df, varName = 'I', varLabel = 'Infected (proportion)', sList = d2)
  mtext(str_to_title(regimeName), side = 3, outer = TRUE)
  dev.off()
}

