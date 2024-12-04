# Research map

# Step 1: create graphical figure of FAR and RR expectations for range of climate-transmission relationships
source('attributable_fraction_framework.R')

# Step 2: create 'normal' climate time series based on thresholds above
# This code requires functions from 'functions_to_simulate_climate.R' and 'time_spans.R' &
# running 'ancillary_climate_data.R'
source('simulate_climate.R')

# Step 3: create times series of betas from climate time series
source('simulate_betas.R')

# Step 4: run models with beta time series
# This step is done on the cluster using 'run_vbd_sim_cluster.R' and 'run_wbd_sim_cluster.R'

# Step 5: determine time points for extreme events based on periods of high and low susceptibility and plot
source('identify_time_points_of_diff_susceptibility.R')

# Step 6: create 'extreme event' climate time series
source('simulate_climate_extremes.R')

# Step 7: create 'extreme event' time series of betas from extreme event climate time series
source('simulate_betas_ee.R')

# Step 8: run model with extreme event beta time series
# This step is done on the cluster using 'run_vbd_sim_cluster.R' and 'run_wbd_sim_cluster.R'

# Step 9: visual inspection of results
source('plot_disease_time_series.R')

# Step 10: calculate summary metrics for model runs (i.e., beta time series)
# ran this step on the cluster, but don't need to
source('analyze_simulations.R')

# Step 13: conduct power analysis to statistically compare normal and extreme event time series
source('power_analyses.R')

# Step 14: combine metrics and perform t-test to statistically compare normal and extreme event time series
source('combine_metrics.R')

# Step 15: create heatmaps of power analyses & t-tests
source('heatmaps.R')

# Step 16: create plots relating extreme events to past outbreaks
source('plot_historical_ee_driven_outbreaks.R')

# Step 17: create conceptual framework plot for relative confidence in attribution
source('scatterplot.R')