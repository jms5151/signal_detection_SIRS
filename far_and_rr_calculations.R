# load libraries
library(rootSolve)
library(ggplot2)
library(tidyverse)

# source functions
source('functions_to_calculate_beta.R')
source('parameter_values.R')

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

vbd_calc_Re <- function(beta, S, params){
  Re = (beta / (params$mu + params$gamma)) * S
  return(Re)
}

wbd_calc_Re <- function(beta, S, params){
  Re = ((params$beta1 + beta) / (params$mu + params$gamma)) * S
  return(Re)
}

generic_sapply <- function(metric, calcfun, ...){
  xx <- sapply(metric, function(x) calcfun(x, ...))
  return(xx)
}

add_re_by_s <- function(df, refun, params){
  df$Low <- generic_sapply(df$p1, refun, S = 0.2, params = params)
  df$High <- generic_sapply(df$p1, refun, S = 0.8, params = params)
  df$Low[df$Low<0] <- 0
  df$High[df$High<0] <- 0
  df$Low[is.na(df$V1)] <- NA
  df$High[is.na(df$V1)] <- NA
  return(df)
}

generate_df <- function(valstotest, betafun, da_metric, dafun, climfun){
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
    climate_df$climval <- rep(valstotest[i])
    climate_df$p1 <- betaextreme
    climate_df$p0 <- rep(beta90s[i], nrow(climate_df))
    df <- rbind(df, climate_df)
    rm(climate_df)
  }
  return(df)
}

map_labels <- function(df, existingcol, existingcolvals, newcolvals){
  for(i in 1:length(existingcolvals)){
    df[df[,existingcol] == existingcolvals[i],'newcol'] <- newcolvals[i]
  }
  return(df$newcol)
}

gen_base_plot_data <- function(x1, x2, stepsize, betafun){
  xseq <- seq(x1, x2, stepsize)
  xbeta <- generic_sapply(xseq, betafun)
  xdf <- data.frame('xseq' = xseq, 'xbeta' = xbeta)
  return(xdf)
}

# basic functional plot
baseplot <- function(df, xvals, yvals, xLabel, yLabel, grouping = NULL, coloring = NULL, lineType = NULL, colorName = NULL, linetypeName = NULL){
  p1 <- ggplot(df, aes(x = df[,xvals], y = df[,yvals], group = grouping, color = coloring, linetype = lineType)) +
    geom_line() +
    theme_classic(base_size = 14) +
    xlab(xLabel) +
    ylab(yLabel) +
    labs(color = colorName, linetype = linetypeName)
  return(p1)
}

updatePlotColors <- function(plot, value_colors) {
  # Update color scale
  plot <- plot + scale_color_manual(values = value_colors)
  return(plot)
}

addvlines <- function(base_plot, values, value_labels, value_colors){
  line_info <- data.frame(
    position = values,
    label = value_labels,
    color = value_colors
  )
  
  # Add vertical lines
  for(i in 1:nrow(line_info)) {
    base_plot <- base_plot +
      geom_vline(xintercept = line_info$position[i], linetype = 'dashed', color = line_info$color[i], linewidth = 1)
  }
  
  base_plot <- base_plot + 
    geom_text(data = line_info, aes(x = position, y = Inf, label = label, color = color), hjust = 1.2, vjust = 1.5, size = 4) +
    scale_color_manual(values = setNames(line_info$color, line_info$label)) +
    guides(color = 'none') # Hide the color guide since it's not needed
  
  return(base_plot)
  
}

# base info
rvalue_labels <- c('Dry', 'Mod', 'Wet')
rvalue_colors <- c('darkorange4', 'lightblue', 'blue')
rainLegLabel <- 'Rainfall Regime'

tvalue_labels <- c('Temperate', 'Warm', 'Hot')
tvalue_colors <- c('orange', 'red', 'darkred')
names(tvalue_colors) <- tvalue_labels
tempLegLabel <- 'Temperature Regime'

# relative risk
rrseq <- seq(from = 0, to = 20, by = 0.5)
# FAR
farseq <- seq(from = 0, to = 0.99, by = 0.01)

# figure directory
figdir <- '../figures/'

# plot functional forms
# WBD/rainfall
rain_values <- c(10, 35, 75)
rdf <- gen_base_plot_data(0, 300, 1, Eisenberg_beta)
rbase_plot <- baseplot(df = rdf, xvals = 'xseq', yvals = 'xbeta', xLabel = 'Rainfall', yLabel = 'Transmission rate')
rplot <- addvlines(rbase_plot, values = rain_values, value_labels = rvalue_labels, value_colors = rvalue_colors)
ggsave(filename = paste0(figdir, 'functional_forms/beta_rainfall.pdf'), plot = rplot)

# VBD/temperature
temp_values <- c(17, 23, 29)
tdf <- gen_base_plot_data(10, 35, 0.1, Lambrechts_beta)
tbase_plot <- baseplot(df = tdf, xvals = 'xseq', yvals = 'xbeta', xLabel = 'Temperature', yLabel = 'Transmission rate')
tplot <- addvlines(tbase_plot, values = temp_values, value_labels = tvalue_labels, value_colors = tvalue_colors)
ggsave(filename = paste0(figdir, 'functional_forms/beta_temperature.pdf'), plot = tplot)

## WBD/rainfall
# FAR
rain_far <- generate_df(valstotest = rain_values
                        , betafun = Eisenberg_beta
                        , da_metric = farseq
                        , dafun = p1_far
                        , climfun = backcalc_rainfall)
rain_far$rain_labels <- map_labels(df = rain_far, existingcol = 'climval', existingcolvals = rain_values, newcolvals = rvalue_labels)

# RR                        
rain_rr <- generate_df(valstotest = rain_values
                       , betafun = Eisenberg_beta
                       , da_metric = rrseq
                       , dafun = p1_rr
                       , climfun = backcalc_rainfall)
rain_rr$rain_labels <- map_labels(df = rain_rr, existingcol = 'climval', existingcolvals = rain_values, newcolvals = rvalue_labels)


# Susceptibility & Re
rain_rr_s <- add_re_by_s(rain_rr, wbd_calc_Re, wbd.params)

rain_rr_long <- rain_rr_s %>%
  pivot_longer(cols = c(Low, High), names_to = "variable", values_to = "Re") %>%
  as.data.frame()


## plots
# FAR plot
far_plot_rain <- baseplot(df = rain_far, xvals = 'V1', yvals = 'metric'
                          , xLabel = 'Rainfall', yLabel = 'FAR'
                          , grouping = rain_far$rain_labels
                          , coloring = rain_far$rain_labels
                          , colorName = rainLegLabel)
far_plot_rain <- updatePlotColors(far_plot_rain, rvalue_colors)
far_plot_rain <- far_plot_rain + xlim(0,300)

# RR plot
rr_plot_rain <- baseplot(df = rain_rr_s, xvals = 'V1', yvals = 'metric'
                 , xLabel = 'Rainfall', yLabel = 'Relative Risk'
                 , grouping = rain_rr$rain_labels
                 , coloring = rain_rr$rain_labels
                 , colorName = rainLegLabel)
rr_plot_rain <- updatePlotColors(rr_plot_rain, rvalue_colors)

# Susceptibility plot
suscep_rain <- baseplot(df = rain_rr_long
                        , xvals = 'Re', yvals = 'metric'
                        , xLabel = 'Re', yLabel = 'Fold-change in\ntransmission rate'
                        , grouping = interaction(rain_rr_long$rain_labels, rain_rr_long$variable)
                        , coloring = rain_rr_long$rain_labels
                        , lineType = rain_rr_long$variable
                        , colorName = rainLegLabel
                        , linetypeName = 'Susceptibility')

suscep_rain <- suscep_rain + 
  geom_vline(xintercept = 1, col = 'grey') +
  scale_color_manual(values = rvalue_colors) 


## VBD/temperature
# FAR
temp_far <- generate_df(valstotest = temp_values
                        , betafun = Lambrechts_beta
                        , da_metric = farseq
                        , dafun = p1_far
                        , climfun = backcalc_temperature)
temp_far$temp_labels <- map_labels(df = temp_far, existingcol = 'climval', existingcolvals = temp_values, newcolvals = tvalue_labels)

temp_far_long <- temp_far %>%
  pivot_longer(cols = c(V1, V2), names_to = "variable", values_to = "V1") %>%
  as.data.frame()

# RR
temp_rr <- generate_df(valstotest = temp_values
                       , betafun = Lambrechts_beta
                       , da_metric = rrseq
                       , dafun = p1_rr
                       , climfun = backcalc_temperature)
temp_rr$temp_labels <- map_labels(df = temp_rr, existingcol = 'climval', existingcolvals = temp_values, newcolvals = tvalue_labels)

temp_rr_long <- temp_rr %>%
  pivot_longer(cols = c(V1, V2), names_to = "variable", values_to = "V1") %>%
  as.data.frame()
  
# Susceptibility
temp_rr_s <- add_re_by_s(temp_rr_long, vbd_calc_Re, vbd.params)

temp_rr_s_long <- temp_rr_s %>%
  pivot_longer(cols = c(Low, High), names_to = "variable2", values_to = "Re") %>%
  as.data.frame()

## Plots
# FAR plot
temp_far_plot <- baseplot(df = temp_far_long
                          , xvals = 'V1'
                          , yvals = 'metric'
                          , grouping = interaction(temp_far_long$temp_labels, temp_far_long$variable)
                          , coloring = temp_far_long$temp_labels
                          , xLabel = 'Temperature'
                          , yLabel = 'FAR'
                          , colorName = tempLegLabel)
temp_far_plot <- temp_far_plot + scale_color_manual(values = tvalue_colors)


# RR plot
temp_rr_plot <- baseplot(df = temp_rr_long
                          , xvals = 'V1'
                          , yvals = 'metric'
                          , grouping = interaction(temp_rr_long$temp_labels, temp_rr_long$variable)
                          , coloring = temp_rr_long$temp_labels
                          , xLabel = 'Temperature'
                          , yLabel = 'Relative Risk'
                          , colorName = tempLegLabel)
temp_rr_plot <- temp_rr_plot + scale_color_manual(values = tvalue_colors) + ylim(0,10)

# Susceptibility plot
suscep_temp <- baseplot(df = temp_rr_s_long
                        , xvals = 'Re', yvals = 'metric'
                        , xLabel = 'Re', yLabel = 'Fold-change in\ntransmission rate'
                        , grouping = interaction(temp_rr_s_long$temp_labels, temp_rr_s_long$variable2)
                        , coloring = temp_rr_s_long$temp_labels
                        , lineType = temp_rr_s_long$variable2
                        , colorName = tempLegLabel
                        , linetypeName = 'Susceptibility')

suscep_temp <- suscep_temp + 
  geom_vline(xintercept = 1, col = 'grey') +
  scale_color_manual(values = tvalue_colors) +
  ylim(0,10)


# # add classification
# probs$classification <- ifelse(round(probs$hist_prob, 2) >= 0.85 & round(probs$hist_prob, 2) <= 1.15, '1-yr event', NA)
# probs$classification[round(probs$hist_prob, 1) == 0.2] <- '5-yr event'
# probs$classification[round(probs$hist_prob, 1) == 0.1] <- '10-yr event'
# probs$classification[round(probs$hist_prob, 2) == 0.02] <- '50-yr event'
# probs$classification[round(probs$hist_prob, 2) == 0.01] <- '100-yr event'
# probs$classification <- factor(probs$classification, levels = c('1-yr event', '5-yr event', '10-yr event', '50-yr event', '100-yr event'))

