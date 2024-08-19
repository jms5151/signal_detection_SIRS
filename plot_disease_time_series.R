## plot simulations
# old


files <- list.files('../results/', full.names = T)
wbdfiles <- files[grepl('wbd', files)]
vbdfiles <- files[grepl('vbd', files)]

pdf('../figures/time_series/wbd.pdf', width = 8, height = 6)
par(mfrow = c(3,2), mar = c(1,4,4,1))
for(i in wbdfiles){
  x <- readRDS(i)
  countryname <- gsub('../results/ee_wbd_|.RData', '', i)
  plot.ts(x$iter1_100I_15D_peak$I, col = 'lightblue', lwd = 3, ylab = c('Infected'), xlab = c(''), main = countryname)
  lines(x$iter1_normal$I)
  mtext(paste0('90th percentile = ', round(quantile(x$iter1_normal$beta[x$iter1_normal$beta>0], 0.90), 2)))
}
dev.off()

pdf('../figures/time_series/vbd.pdf', width = 8, height = 6)
par(mfrow = c(3,2), mar = c(1,4,4,1))
for(i in vbdfiles){
  x <- readRDS(i)
  countryname <- gsub('../results/ee_vbd_|.RData', '', i)
  plot.ts(x$iter1_10I_40D_peak$I, col = 'orange', lwd = 3, ylab = c('Infected'), xlab = c(''), main = countryname)
  lines(x$iter1_normal$I)
  mtext(paste0('90th percentile = ', round(quantile(x$iter1_normal$beta[x$iter1_normal$beta>0], 0.90), 2)))
}
dev.off()

# plot normal vs ee simulations for moderate climate regime
n <- readRDS('../data/sim_results/normal_warm.RData')
ee_Smax <- readRDS('../data/sim_results_ee/S_max_t_warm.RData')
ee_Smin <- readRDS('../data/sim_results_ee/S_min_t_warm.RData')

plot.ts(n$normal_warm_1$I, ylab = 'Infected', lwd = 2, main = 'High susceptibility')
lines(ee_Smax$warm_1_10I_7D$I, col = 'red', lwd = 2)

plot.ts(n$normal_warm_1$I, ylab = 'Infected', lwd = 2, main = 'Low susceptibility')
lines(ee_Smin$warm_1_10I_7D$I, col = 'red', lwd = 2, lty = 2)

plot(ee_Smax$warm_1_10I_7D$I, ee_Smax$warm_1_10I_7D$S, type = 'l', col = 'red', xlab = 'Susceptible', ylab = 'Infected')
lines(n$normal_warm_1$I, n$normal_warm_1$S, type = 'l')

plot(ee_Smin$warm_1_10I_7D$I, ee_Smin$warm_1_10I_7D$S, lwd = 2, lty = 2)
lines(n$normal_warm_1$I, n$normal_warm_1$S, type = 'l')
