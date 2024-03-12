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


x2 <- x[grepl('normal', x)]
