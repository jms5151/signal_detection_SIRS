# Plot normal vs extreme event simulations for warm climate regime
n <- readRDS('../data/sim_results/normal_warm.RData')
ee_Smax <- readRDS('../data/sim_results_ee/S_max_t_warm.RData')
ee_Smin <- readRDS('../data/sim_results_ee/S_min_t_warm.RData')

# Main plot
pdf('../figures/time_series/disease_ee_ts.pdf', width = 8, height = 5)
par(mar = c(4, 4, 2, 1), mgp = c(2, 0.5, 0), las = 1)
plot.ts(n$normal_warm_20$I, ylab = 'Infected', lwd = 4, ylim = c(0, 0.7))
lines(ee_Smax$warm_20_4I_14D$I, col = 'red', lwd = 2)
lines(ee_Smin$warm_20_4I_14D$I, col = 'grey', lwd = 2, lty = 2)

# Add legend
legend('topleft', legend = c('Normal', 'High susceptibility', 'Low susceptibility'), 
       col = c('black', 'red', 'grey'), lwd = c(4, 2, 2), lty = c(1, 1, 2), bty = 'n')

# Add the phase plot in the upper right corner
par(fig = c(0.69, 0.99, 0.45, 0.97), new = TRUE)
plot(ee_Smax$warm_20_4I_14D$I, ee_Smax$warm_10_4I_15D$S, col = 'red', lwd = 2, type = 'l', xlab = 'Infected', ylab = 'Susceptible')
lines(ee_Smin$warm_20_4I_14D$I, ee_Smin$warm_10_4I_15D$S, col = 'grey', lwd = 2, lty = 2)
lines(n$normal_warm_20$I, n$normal_warm_10$S)
dev.off()

# # wbd
# n <- readRDS('../data/sim_results/normal_dry.RData')
# ee_Smax <- readRDS('../data/sim_results_ee/S_max_t_dry.RData')
# ee_Smin <- readRDS('../data/sim_results_ee/S_min_t_dry.RData')
# 
# plot.ts(n$normal_dry_3$I, ylab = 'Infected', lwd = 2, main = 'High susceptibility')
# lines(ee_Smax$dry_3_100I_5D$I, col = 'red', lwd = 2)
# 
# plot.ts(n$normal_moderate_1$I, ylab = 'Infected', lwd = 2, main = 'Low susceptibility')
# lines(ee_Smin$dry_13_100I_5D$I, col = 'red', lwd = 2, lty = 2)
