x <- readRDS('../output/ee_sumarized_vbd_Brazil_window_30.RData')


#### OLD
# calculate power and format
calcPower <- function(x){
  m1 = x$Mean[x$Group == 'normal']
  s1 = x$SD[x$Group == 'normal']
  x$Power = NA
  for(i in 2:nrow(x)){
    p <- pwrss.t.2means(mu1 = m1
                        , mu2 = x$Mean[i]
                        , sd1 = s1
                        , sd2 = x$SD[i]
                        , kappa = 1 # kappa = sample size 1 / sample size 2
                        , n2 = Iterations
                        , alpha = 0.05
                        , alternative = "not equal")
    x$Power[i] <- p$power
  }
  return(x)
}

# plot.ts(brazil_sir_out[[1]]$I)
# for(i in 2:Iterations){
#   lines(brazil_sir_out[[i]]$I)
# }
# abline(v = t1)
# lines(brazil_sir_out[[600]]$I, col = 'lightblue')
# abline(v = t1 + 100, col = 'red')
# abline(v = t1 + 160, col = 'purple')
# abline(v = t1 + 200, col = 'blue')

library(stringr)

heatFormat <- function(x, timing){
  ids <- grep(timing, x$Group)
  x3 <- x[ids,]
  x3 <- cbind(x3, do.call(rbind, str_split(x3$Group, '_')))
  colnames(x3)[5:6] <- c('temp', 'dur')
  x3$temp <- gsub('C', '', x3$temp)
  x3$dur <- gsub('d', '', x3$dur)
  x3[,c('temp', 'dur')] <- lapply(x3[,c('temp', 'dur')], as.numeric)
  # x4 <- unstack(x3[,c('temp', 'dur', 'Power')], Power ~ temp)
  # rownames(x4) <- sort(unique(x3$dur))
  return(x3)
}

# get group names (remove iteration identifier)
groups <- unique(gsub('^.*?_', '', names(TS_BANGL)))

# identify the time frame of interest based on when the extreme event occurred
x <- unlist(lapply(ee.vbd.br.temp[1:Iterations], function(x) which(x == max(x[time1:time2]))))
t1 = min(x) - 45
t2 = t1 + 180

# create functions for metric calculation
groupFunSDBETA <- function(x){sd(x$beta[t1:t2])}
groupFunSDRe <- function(x){sd(x$Re[t1:t2])}
groupFunFS <- function(x){sum(x$I[t1:t2])}

groupfuns <- list(groupFunSDBETA, groupFunSDRe, groupFunFS)
# names(groupfuns) <- c('sd_beta', 'sd_Re', 'final_size')

heatmapdf <- function(sirout, groupfun, nameID){
  x <- sumSIR(sirout = sirout, groupfun = groupfun)
  x2 <- calcPower(x = x)
  for(i in ExEV_times){
    x3 <- heatFormat(x = x2, timing = i)
    write.csv(x3, paste0('../output/heatmap_', nameID, '_', i, '.csv'), row.names = F)
  }
}

library(pwrss)
heatmapdf(sirout = brazil_sir_out, groupfun = groupFunSDBETA, nameID = 'Brazil_sd_beta')
heatmapdf(sirout = brazil_sir_out, groupfun = groupFunSDRe, nameID = 'Brazil_sd_Re')
heatmapdf(sirout = brazil_sir_out, groupfun = groupFunFS, nameID = 'Brazil_final_size')


heatmapdf(sirout = bangladesh_sir_out, groupfun = groupFunSDBETA, nameID = 'Bangladesh_sd_beta')
heatmapdf(sirout = bangladesh_sir_out, groupfun = groupFunSDRe, nameID = 'Bangladesh_sd_Re')
heatmapdf(sirout = bangladesh_sir_out, groupfun = groupFunFS, nameID = 'Bangladesh_final_size')

library(ggplot2)
library(RColorBrewer)

myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
sc <- scale_fill_gradientn(colours = myPalette(100))

powerHeatMap <- function(df, titleName){
  ggplot(df, aes(y = dur, x = temp, fill = Power)) + 
    geom_tile() +
    sc +  
    labs(fill="Power", x = 'Temperature (degrees C)', y = 'Duration (days)') +
    theme_classic() +
    ggtitle(titleName)
}

files <- list.files('../output/', full.names = T)
heatFiles <- files[grep('heatmap', files)]

for(i in heatFiles){
  dfheat <- read.csv(i)
  plotName <- gsub('../output/heatmap_|.csv', '', i)
  plotName <- gsub('_', ' ', plotName)
  plotheat <- powerHeatMap(df = dfheat, titleName = plotName)
  newfilepath <- gsub('output', 'figures', i)
  newfilepath <- gsub('.csv', '.pdf', newfilepath)
  ggsave(plotheat, file = newfilepath)
}

