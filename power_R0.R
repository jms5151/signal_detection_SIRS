library(ggplot2)
library(reshape2)
library(gridExtra)
library(ggpubr)
library(treemapify)
library(RColorBrewer)

# Simple example comparing extreme temp. event raising baseline R0 from 3 to 5
N = 1e6
gamma = 1/5
mu = 1/365
R0_normal = 3
R0_extreme = 5
beta_normal = (R0_normal / N) * (gamma + mu)
beta_extreme = (R0_extreme / N) * (gamma + mu)
sd.error.normal = beta_normal * 0.01
sd.error.extreme = beta_extreme * 0.01
ninetyfivepct.R0_normal = (N / (gamma + mu)) * beta_normal + 1.645 * ((N * sd.error.normal) / (gamma + mu))
pct.extreme.dist.under.ninetyfivepct.R0_normal <- pnorm(ninetyfivepct.R0_normal, mean = (N / (gamma + mu)) * beta_extreme, sd = (N / (gamma + mu)) * sd.error.extreme, lower.tail = TRUE, log.p = FALSE)
power = 1 - pct.extreme.dist.under.ninetyfivepct.R0_normal
power

# Plot showing power when varying the SD of the error and the R0 of the extreme event (assuming R0 
# of the normal years is always 3)
vary_SD = seq(6.082192e-08, 6.082192e-07, 1e-08)
vary_R0_extreme = seq(3, 10, 0.01) # Say that extreme weather events can at most double the R0
res = c()
for (R0_extreme_input in vary_R0_extreme) {
  for (sd_input in vary_SD) {
    R0_normal = 3
    R0_extreme = R0_extreme_input
    beta_normal = (R0_normal / N) * (gamma + mu)
    beta_extreme = (R0_extreme / N) * (gamma + mu)
    sd.error.normal = sd_input
    sd.error.extreme = sd_input
    ninetyfivepct.R0_normal = (N / (gamma + mu)) * beta_normal + 1.645 * ((N * sd.error.normal) / (gamma + mu))
    pct.extreme.dist.under.ninetyfivepct.R0_normal <- pnorm(ninetyfivepct.R0_normal, mean = (N / (gamma + mu)) * beta_extreme, sd = (N / (gamma + mu)) * sd.error.extreme, lower.tail = TRUE, log.p = FALSE)
    power = 1 - pct.extreme.dist.under.ninetyfivepct.R0_normal
    res = c(res, power)
  }
}
finalMat = matrix(res, ncol=length(vary_SD), nrow=length(vary_R0_extreme), byrow=T)
rownames(finalMat) <- vary_R0_extreme
colnames(finalMat) <- round((as.numeric(vary_SD) / 6.082192e-07) * 100)
finalMat = data.frame(finalMat, check.names = F)
finalMat$id = vary_R0_extreme #seq(1, length(vary_R0_extreme), by=1)
melted <- melt(finalMat, id.var='id')
colnames(melted) <- c('vary_R0_extreme', 'vary_SD', 'value')
myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
sc <- scale_fill_gradientn(colours = myPalette(100))
temp_plot <- ggplot(melted, aes(y = vary_R0_extreme, x = vary_SD, fill = value)) + geom_tile() +
   sc+  labs(fill="Power") +
  ylab('R0, extreme') + xlab('SD of beta (% of baseline beta) (assuming same for normal and extreme)') + ggtitle('')
temp_plot

# Plot showing line of power ---------------------------------------------------
xs <- c()
ys <- c()
for (SD_dex in 1:length(vary_SD)) {
  xs <- c(xs, round((as.numeric(vary_SD[SD_dex]) / 6.082192e-07) * 100))
  ys <- c(ys, (vary_R0_extreme[tail(which(finalMat[,SD_dex] < 0.8), 1) + 1] / 3))
}
plot(xs, ys, type='l', lwd=3, ylab='...times the baseline R0 (as well as baseline beta)', xlab='SD = % of baseline beta')



# Plotting for debugging -------------------------------------------------------
population_mean <-  (N / (gamma + mu)) * beta_normal
population_sd <-  (N / (gamma + mu)) * sd.error.normal

#define upper and lower bound
lower_bound <- population_mean - population_sd
upper_bound <- population_mean + population_sd

x <- seq(-4, 4, length = 1000) * population_sd + population_mean

#create a vector of values that shows the height of the probability distribution
#for each value in x
y <- dnorm(x, population_mean, population_sd)

#plot normal distribution with customized x-axis labels
plot(x,y, type = "l", lwd = 2, axes = FALSE, xlab = "", ylab = "")
sd_axis_bounds = 5
axis_bounds <- seq(-sd_axis_bounds * population_sd + population_mean,
                   sd_axis_bounds * population_sd + population_mean,
                   by = population_sd)
axis(side = 1, at = axis_bounds, pos = 0)

## Jamie's addtion/update
library(ggplot2)
library(reshape2)
library(gridExtra)
library(ggpubr)
library(treemapify)
library(RColorBrewer)

r0_normal_95 <- function(N, gamma, beta, sd){
  (N / gamma) * beta + 1.645 * ((N * sd) / gamma)
}

percentExtreme <- function(N, gamma, r0_normal, beta_extreme, sd_extreme){
  pnorm(r0_normal
        , mean = (N / gamma) * beta_extreme
        , sd = (N / gamma) * sd_extreme
        , lower.tail = TRUE
        , log.p = FALSE)
}

analyticalPower <- function(N, gamma, meanR0, maxR0, title){
  mu = 0
  beta_normal = (meanR0 / N) * (gamma + mu)
  
  varyBetaMultiplier = seq(1, 4, by = 0.1)
  vary_SD = beta_normal * varyBetaMultiplier
  vary_R0_extreme = seq(meanR0, maxR0, 0.5) 
  
  res = c()
  for (R0_extreme_input in vary_R0_extreme) {
    for (sd_input in vary_SD) {
      R0_normal = meanR0
      R0_extreme = R0_extreme_input
      beta_normal = (R0_normal / N) * (gamma + mu)
      beta_extreme = (R0_extreme / N) * (gamma + mu)
      sd.error.normal = sd_input #beta_normal
      sd.error.extreme = sd_input
      ninetyfivepct.R0_normal = (N / (gamma + mu)) * beta_normal + 1.645 * ((N * sd.error.normal) / (gamma + mu))
      pct.extreme.dist.under.ninetyfivepct.R0_normal <- pnorm(ninetyfivepct.R0_normal, mean = (N / (gamma + mu)) * beta_extreme, sd = (N / (gamma + mu)) * sd.error.extreme, lower.tail = TRUE, log.p = FALSE)
      power = 1 - pct.extreme.dist.under.ninetyfivepct.R0_normal
      res = c(res, power)
    }
  }
  finalMat = matrix(res, ncol=length(vary_SD), nrow=length(vary_R0_extreme), byrow=T)
  rownames(finalMat) <- vary_R0_extreme
  colnames(finalMat) <- round((as.numeric(vary_SD) / beta_normal), 2)
  finalMat = data.frame(finalMat, check.names = F)
  finalMat$id = vary_R0_extreme #seq(1, length(vary_R0_extreme), by=1)
  melted <- melt(finalMat, id.var='id')
  colnames(melted) <- c('vary_R0_extreme', 'vary_SD', 'value')
  myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
  sc <- scale_fill_gradientn(colours = myPalette(100))
  
  ggplot(melted, aes(y = vary_R0_extreme, x = vary_SD, fill = value)) + geom_tile() +
    sc+  labs(fill="Power") +
    ylab('R0, extreme') + xlab('Beta variability (assuming same for normal and extreme)') + ggtitle('') +
    theme_classic() +
    ggtitle(title)
}

dengue <- read.csv('data/dengue_R0_historical.csv')
dengue <- subset(dengue, R0 < 30) # remove extreme outlier

dengue.meanR0 <- round(mean(dengue$R0))
dengue.sdR0 <- sd(dengue$R0)
dengue.maxR0 <- round(max(dengue$R0))

dengue.N = 10000
dengue.gamma = 1/15 # dengue

cholera.N = 1 # cholera

cholera.gamma = 1/4 # cholera
cholera.meanR0 <- 2.15 # cholera
cholera.maxR0 <- 18 # cholera
# mu = 0

denguePower <- analyticalPower(N = dengue.N, gamma = dengue.gamma, meanR0 = dengue.meanR0, maxR0 = dengue.maxR0, title = 'VBD')
choleraPower <- analyticalPower(N = cholera.N, gamma = cholera.gamma, meanR0 = cholera.meanR0, maxR0 = cholera.maxR0, title = 'WBD')
