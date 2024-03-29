# Identify times of low and high population susceptibility for extreme event perturbations

# list filepaths
sim_data_path <- '../data/sim_results/'
dis_files <- list.files(sim_data_path, full.names = T)

# function to determine min/max susceptibility and timing
Sid <- function(d, t1, t2){
  x2 <- lapply(d, function(x) list('S_min' = min(x$S[t1:t2])
                                   , 'S_min_t' = which(x$S == min(x$S[t1:t2]))
                                   , 'S_max' = max(x$S[t1:t2])
                                   , 'S_max_t' = which(x$S == max(x$S[t1:t2]))))
  return(x2)
}

# run loop to extract min/max susceptibility values and timing, save output 
for(i in 1:length(dis_files)){
  d <- readRDS(dis_files[i])
  regimeName <- gsub('../data/sim_results/normal_|.RData', '', dis_files[i])
  if(regimeName == 'temperate'){
    t1 = 31
    t2 = 300
  } else {
    t1 = 300
    t2 = 600
  }
  d2 <- Sid(d, t1, t2)
  newfilepath <- gsub('sim_results/', 'sim_sids/sid_', dis_files[i])
  saveRDS(d2, file = newfilepath)
}
