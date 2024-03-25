# create beta lists

# source functions
source('functions_to_calculate_beta.R')

# filepaths
sourcedir <- '../data/sim_climate/'

# list names of climate lists
clim_files <- list.files(sourcedir, full.names = T)

# load climate data, translate to beta lists, save
for(i in clim_files){
  x_clim <- readRDS(i)
  if(grepl('temperate|warm|hot', i)){
    x_beta <- lapply(x_clim, function(x) Lambrechts_beta(x)) 
  } else if(grepl('dry|moderate|wet', i)){
    x_beta <- lapply(x_clim, function(x) Eisenberg_beta(x)) 
  }
  newfilepath <- gsub('climate|clim', 'betas', i)
  saveRDS(x_beta, file = newfilepath)
}

