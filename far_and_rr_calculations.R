# load libraries
library(rootSolve)
library(ggplot2)
library(tidyverse)

# source functions
source('functions_to_calculate_beta.R')

# custom functions
far <- function(p0, p1){ (p1 - p0)/p1 }

rr <- function(p0, p1){ p1/p0 }

p1_far <- function(x,k){ -k/(x-1) }

p1_rr <- function(x,k){ x*k }

backcalc_rainfall <- function(x){ x/0.00128 }

revLambrechts <- function(y, t){
  b0 = 1.14492
  res = 0.001044 * t * (t - 12.286) * sqrt(32.461 - t) 
  res = b0 * (res * res) - y
  return(res)
}

backcalc_temperature <- function(x, t){
  uniroot.all(function(t) { revLambrechts(x, t) }, c(12.286, 32.461))
}

backcalc_rainfall <- function(x){x/0.00128}


generic_sapply <- function(metric, calcfun, ...){
  xx <- sapply(metric, function(x) calcfun(x, ...))
  return(xx)
}

get_colors <- function(x){
  h = seq(250, 10, length.out = length(x))
  linecols <- hcl(h, c = 100, l = 65)
  return(linecols)
}

generate_df <- function(valstotest, betafun, da_metric, dafun, climfun, linecols){
  df <- data.frame()
  beta90s <- generic_sapply(valstotest, betafun)
  for(i in 1:length(beta90s)){
    betaextreme <- generic_sapply(da_metric, dafun, k = beta90s[i])
    climate <- generic_sapply(betaextreme, climfun)
    if(is.matrix(climate) == T){
      climate_df <- as.data.frame(cbind('V1' = climate[1,], 'V2' = climate[2,]))
    } else {
      climate_df <- lapply(climate, function(x) if(length(x) == 0) c(NA, NA) else x)
      climate_df <- as.data.frame(t(do.call(cbind, climate_df)))
    }
    climate_df$metric <- da_metric
    climate_df$p1 <- betaextreme
    climate_df$p0 <- rep(beta90s[i], nrow(climate_df))
    climate_df$lineColor <- rep(linecols[i], nrow(climate_df))
    df <- rbind(df, climate_df)
    rm(climate_df)
  }
  return(df)
}

pltfun <- function(df, xLabel, yLabel, xmin, xmax, ymin, ymax, grouping){
  ggplot(df, aes(x = V1, y = metric, group = grouping, color = lineColor)) + 
    geom_line(size = 1.1) + 
    scale_color_manual(values = unique(df$lineColor)) + #c('maroon', 'orange', 'lightblue')
    theme_classic() + 
    xlab(xLabel) +
    ylab(yLabel) + 
    xlim(xmin, xmax) +
    ylim(ymin, ymax)
}

# run

farseq <- seq(from = 0, to = 1, by = 0.01)

rain_values <- c(10, 35, 75)
temp_values <- c(17, 23, 29)

rlineColors <- get_colors(rain_values)
tlineColors <- get_colors(temp_values)

# basic functional plot
tseq <- seq(10,35,0.1)
tbeta <- sapply(tseq, function(x) Lambrechts_beta(x))

plot(tseq, tbeta, type = 'l', xlab = 'Temperature', ylab = 'Transmission rate')
for(v in 1:length(temp_values)){
  abline(v = c(temp_values[v]), col = tlineColors[v], lty = 2)
}

rseq <- seq(0,300,1)
rbeta <- sapply(rseq, function(x) Eisenberg_beta(x))

plot(rseq, rbeta, type = 'l', xlab = 'Rainfall', ylab = 'Transmission rate')
for(v in 1:length(rain_values)){
  abline(v = c(rain_values[v]), col = rlineColors[v], lty = 2)
}

# relative risk
rrseq <- seq(from = 0, to = 20, by = 0.5)

rain_rr <- generate_df(valstotest = rain_values
                       , betafun = Eisenberg_beta
                       , da_metric = rrseq
                       , dafun = p1_rr
                       , climfun = backcalc_rainfall,
                       linecols = rlineColors)

temp_rr <- generate_df(valstotest = temp_values
                       , betafun = Lambrechts_beta
                       , da_metric = rrseq
                       , dafun = p1_rr
                       , climfun = backcalc_temperature
                       , linecols = tlineColors)


pltfun(rain_rr, 'Rainfall', 'Relative Risk', grouping = rain_rr$p0, 0, 350, 0, 20)

temp_rr_long <- test %>%
  pivot_longer(cols = c(V1, V2), names_to = "variable", values_to = "V1")

pltfun(temp_rr_long, 'Temperature', 'Relative Risk', grouping = interaction(temp_rr_long$p0, temp_rr_long$variable), 12, 35, 0, 20)

# FAR
farseq <- seq(from = 0, to = 0.99, by = 0.01)

rain_far <- generate_df(valstotest = rain_values
                        , betafun = Eisenberg_beta
                        , da_metric = farseq
                        , dafun = p1_far
                        , climfun = backcalc_rainfall
                        , linecols = rlineColors)

temp_far <- generate_df(valstotest = temp_values
                        , betafun = Lambrechts_beta
                        , da_metric = farseq
                        , dafun = p1_far
                        , climfun = backcalc_temperature
                        , linecols = tlineColors)


pltfun(rain_far, 'Rainfall', 'FAR', grouping = rain_far$p0, 0, 350, 0, 1)

temp_far_long <- temp_far %>%
  pivot_longer(cols = c(V1, V2), names_to = "variable", values_to = "V1")

pltfun(temp_far_long, 'Temperature', 'FAR', grouping = interaction(temp_far_long$p0, temp_far_long$variable), 12, 35, 0, 1)


# # add classification
# probs$classification <- ifelse(round(probs$hist_prob, 2) >= 0.85 & round(probs$hist_prob, 2) <= 1.15, '1-yr event', NA)
# probs$classification[round(probs$hist_prob, 1) == 0.2] <- '5-yr event'
# probs$classification[round(probs$hist_prob, 1) == 0.1] <- '10-yr event'
# probs$classification[round(probs$hist_prob, 2) == 0.02] <- '50-yr event'
# probs$classification[round(probs$hist_prob, 2) == 0.01] <- '100-yr event'
# 
# probs$classification <- factor(probs$classification, levels = c('1-yr event', '5-yr event', '10-yr event', '50-yr event', '100-yr event'))

