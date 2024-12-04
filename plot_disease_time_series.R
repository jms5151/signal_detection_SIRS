# load libraries
library(ggplot2)
library(cowplot)
library(ggpubr)

# plotting function
plot_ts_with_pp <- function(normal, Smax, Smin, yMax = 1, mainTitle = '', addLegend = FALSE){
  main_plot <- ggplot() +
    geom_line(data = normal, aes(x = seq_along(I), y = I, color = 'Normal'), size = 1.5) +
    geom_line(data = Smax, aes(x = seq_along(I), y = I, color = 'High susceptibility'), size = 1) +
    geom_line(data = Smin, aes(x = seq_along(I), y = I, color = 'Low susceptibility'), size = 1, linetype = 'longdash') +
    labs(y = 'Infected', x = 'Time', title = mainTitle) +
    ylim(0, yMax) +
    scale_color_manual(values = c('Normal' = 'black', 'High susceptibility' = '#f00745', 'Low susceptibility' = '#0e9e97')) +
    theme_bw()
  
  if(addLegend == T){
    main_plot <- main_plot + 
      theme(legend.position = c(0.1, 0.9)
            , legend.title = element_blank()
            , legend.background = element_blank()
            )
    drawplotx <- 0.68
  } else {
    main_plot <- main_plot + theme(legend.position = 'non')
    drawplotx <- 0.08
  }

  # Create the phase plot using ggplot2
  phase_plot <- ggplot(Smax, aes(x = S, y = I)) +
    geom_path(color = '#f00745', size = 1) +
    geom_path(data = Smin, aes(x = S, y = I), color = '#0e9e97', linewidth = 1, linetype = 'longdash') +
    geom_path(data = normal, aes(x = S, y = I)) +
    labs(x = 'Susceptible', y = 'Infected') +
    theme_bw()
  
  # Combine the main plot with the phase plot as an inset
  
  combined_plot <- ggdraw() +
    draw_plot(main_plot) +
    draw_plot(phase_plot, x = drawplotx, y = 0.55, width = 0.3, height = 0.35)
}

# WBD
w_n <- readRDS('../data/sim_results/normal_dry.RData')
w_ee_Smax <- readRDS('../data/sim_results_ee/S_max_t_dry.RData')
w_ee_Smin <- readRDS('../data/sim_results_ee/S_min_t_dry.RData')

wbd_plot <- plot_ts_with_pp(normal = as.data.frame(w_n)
                            , Smax = w_ee_Smax$dry_100I_7D
                            , Smin = w_ee_Smin$dry_100I_7D
                            , yMax = 0.25
                            , mainTitle = 'A. Water-borne disease'
                            , addLegend = TRUE
                            ) 


# VBD
n <- readRDS('../data/sim_results/normal_warm.RData')
ee_Smax <- readRDS('../data/sim_results_ee/S_max_t_warm.RData')
ee_Smin <- readRDS('../data/sim_results_ee/S_min_t_warm.RData')

vbd_plot <- plot_ts_with_pp(normal = as.data.frame(n)
                , Smax = ee_Smax$warm_15I_20D
                , Smin = ee_Smin$warm_15I_20D
                , yMax = 1
                , mainTitle = 'B. Vector-borne disease'
                )

# combine and save
combined_plot <- ggarrange(wbd_plot, vbd_plot, ncol = 2)
ggsave(filename = '../figures/time_series/combined_dis_ee_ts.pdf', plot = combined_plot, width = 16, height = 5)
