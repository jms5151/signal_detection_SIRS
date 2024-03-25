## plot simulations

# load data
clim_data_path <- '../data/sim_climate/'
sim_data_path <- '../data/sim_results/'

clim_files <- list.files(clim_data_path, full.names = T)
dis_files <- list.files(sim_data_path, full.names = T)

for(j in 1:length(clim_files)){
  c <- readRDS(clim_files[j])
  d <- readRDS(dis_files[j])
  lineCols <- rainbow(n = length(c))
  titleName <- gsub('../data/sim_climate/normal_|_clim.RData', '', clim_files[j])
  fileName <- paste0('../figures/time_series/sim_', titleName, '.pdf')
  if(titleName == 'wet' | titleName == 'dry' | titleName == 'moderate'){
    yrange = c(0, 400)
  } else {
    yrange = c(-20, 35)
  }
  pdf(fileName, width = 5, height = 6)
  par(mfrow = c(3,1), mar = c(1,4,2,1))
  # climate plot
  plot(c[[1]], type = 'l', main = titleName, ylab = 'Climate', ylim = yrange)
  for(k in 1:length(d)){lines(c[[k]], col = lineCols[k])}
  # infected plot
  plot(d[[1]]$I, type = 'l', ylim = c(0, max(d[[1]]$I)+0.03), ylab = 'Infected', xlab = '')
  for(l in 1:length(d)){lines(d[[l]]$I, col = lineCols[l])}
  # susceptilbe plot
  plot(d[[1]]$S, type = 'l', ylim = c(0, 1), ylab = 'Susceptible', xlab = '')
  for(m in 1:length(d)){lines(d[[m]]$S, col = lineCols[m])}
  dev.off()
}

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

