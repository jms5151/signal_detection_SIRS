# Research map

# Step 1: create scatterplot of diseases across spectrum of predictability
source('csid_scatterplot.R')

# Step 2: create graphical figure of FAR and RR expectations for two systems
source('far_and_rr_calculations.R')

# Step 3: determine thresholds for extreme events given global climate regimates
source('ancillary_climate_data.R')

# Step 4: create 'normal' climate time series based on thresholds above
# This code requires functions from 'functions_to_simulate_climate.R' and 'time_spans.R'
source('simulate_climate.R')

# Step 5: create times series of betas from climate time series
source('simulate_betas.R')

# Step 6: run models with beta time series
# This step is done on the cluster using 'run_vbd_sim_cluster.R' and 'run_wbd_sim_cluster.R'
# visual inspection of results in 'plot_time_series.R'

# Step 7: determine time points for extreme events based on periods of high and low susceptibility
source('identify_time_points_of_diff_susceptibility.R')

# Step 8: create 'extreme event' climate time series
source('simulate_climate_extremes.R')

# Step 9: create 'extreme event' time series of betas from extreme event climate time series
# use second source directory
source('simulate_betas.R')

# Step 10: run model with extreme event beta time series
# This step is done on the cluster using 'run_vbd_sim_cluster.R' and 'run_wbd_sim_cluster.R'
# visual inspection of results in 'plot_time_series.R'

# Step 11: calculate summary metrics for model runs (i.e., beta time series)
# ran this step on the cluster, but don't need to
source('analyze_simulations.R')

# Step 12: conduct power analysis to statistically compare normal and extreme event time series
source('power_analyses.R')

# Step 13: create heatmaps of power analyses
source('heatmaps.R')