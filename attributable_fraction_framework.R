# Create transmission functions, calculate attributable fraction, 
# assess susceptibility affects


# Create climate-beta data from different transmission functions ---------------

# source functions that describe the relationship between climate and transmission
source('transmission_functions.R')
source('far_and_rr_functions.R')
# Define the climate range
x_range <- seq(1, 20, by = 0.1)

# Create data for each functional form
df_linear <- data.frame(Climate = x_range, beta = normalize_function(linear_function, x_range), func = 'Linear (increasing)')
df_linear2 <- data.frame(Climate = x_range, beta = normalize_function(linear_function, x_range, a = -0.1), func = 'Linear (decreasing)')
df_quadratic <- data.frame(Climate = x_range, beta = normalize_function(quadratic_function, x_range), func = 'Quadratic') # a > 1, U-shaped, a < 1 = concave down
df_exp_growth <- data.frame(Climate = x_range, beta = normalize_function(exponential_growth_function, x_range), func = 'Exponential Growth')
df_exp_decay <- data.frame(Climate = x_range, beta = normalize_function(exponential_decay_function, x_range), func = 'Exponential Decay')
df_modified_briere <- data.frame(Climate = x_range, beta = normalize_function(modified_briere, x_range), func = 'Modified Briere')
df_michaelis_menten <- data.frame(Climate = x_range, beta = normalize_function(michaelis_menten, x_range), func = 'Michaelis Menten')

# combine data
df_all <- rbind(df_linear, df_linear2, df_quadratic, df_exp_growth, df_exp_decay, df_modified_briere, df_michaelis_menten)

# Calculate attributable fraction from climate-transmission relationships above ---------


# Define values for calculations/visualizations 

# Define susceptibility levels
s_low = 0.2
s_high = 0.8


# add functional form and threshold information
incr <- c('farthest', 'mid', 'closest')
decr <- c('closest', 'mid', 'farthest')

threshold_data <- data.frame(
  func = rep(c('Exponential Decay', 'Exponential Growth', 'Linear (increasing)', 'Linear (decreasing)', 'Michaelis Menten', 'Modified Briere', 'Quadratic'), each = 3),
  threshold = c(3, 5, 9, 8, 13, 18, rep(c(5, 9, 15), 2), 3, 7, 12, 6, 10, 16, 3, 7, 18),
  # threshold_level = rep(c('low', 'mod', 'high'), length(unique(df_all$func)))
  threshold_level = c(decr, incr, decr, incr, incr, incr, decr) 
)

# create empty data frame
df <- data.frame()

# calculate attributable fractions and Re
for(i in 1:nrow(threshold_data)){
  data <- subset(df_all, func == threshold_data$func[i])
  x_thrhds <- unique(threshold_data$threshold[threshold_data$func == threshold_data$func[i]])
  for(j in x_thrhds){
    df2 <- data
    df2$threshold_level <- threshold_data$threshold_level[threshold_data$func == threshold_data$func[i] & threshold_data$threshold == j]
    df2$threshold <- j
    beta_exceed <- df2$beta[which(df2$Climate == j)]
    df2$FAR <- calc_FAR(p0 = beta_exceed, p1 = df2$beta) 
    df2$RR <- calc_RR(p0 = beta_exceed, p1 = df2$beta)
    df2$AF <- calc_AF(FAR = df2$FAR, RR = df2$RR)
    df2$Low <- Re_calc(beta = df2$beta, S = s_low)
    df2$High <- Re_calc(beta = df2$beta, S = s_high)
    df <- rbind(df, df2)
  }
}

df$climate_delta <-  df$Climate - df$threshold

# Format
dfx <- subset(df, AF > 0)
dfx$threshold_level <- factor(dfx$threshold_level, levels = c('farthest', 'mid', 'closest'))

long_data_s <- df %>%
  pivot_longer(cols = c(Low, High), names_to = 'Susceptibility', values_to = 'Re') %>%
  as.data.frame()


# Plot -------------------------------------------
# set colors for thresholds
value_colors <- c('low' = 'darkorange4', 'mod' = 'lightblue', 'high' = 'blue')
value_colors <- c('farthest' = 'darkorange4', 'mid' = 'lightblue', 'closest' = 'blue')

# generic functions plot
functions_plot <- ggplot(df_all, aes(x = Climate, y = beta)) +
  geom_vline(data = threshold_data, aes(xintercept = threshold, col = threshold_level), size = 1.1) +
  geom_line(linewidth = 1) +
  facet_wrap(~func, nrow = 1) + # 
  labs(title = 'Transmission function',
       x = 'Climate variable',
       y = 'Transmission rate') +
  theme_bw(base_size = 14) +
  scale_color_manual(values = value_colors) +
  # theme(legend.position = 'top', legend.background = element_rect(fill='transparent')) 
  guides(color = 'none')

af_plot <- ggplot(dfx, aes(x = climate_delta, y = AF, color = threshold_level)) +
  geom_line(size = 1.1) +
  theme_bw(base_size = 14) +
  facet_wrap(~func, nrow = 1) + 
  labs(title = 'Climate attribution',
       x = 'Extreme event caused climate deviation',
       y = 'Attributable fraction'
  ) +
  scale_color_manual(values = value_colors,
                     name = 'Climate regime suitability',  # Custom title for the legend
                     labels = c('High', 'Moderate', 'Low')  # Custom labels for the legend
  ) + 
  # guides(color = 'none') +
  theme(legend.position = 'bottom', 
        legend.background = element_rect(fill='transparent'),
        strip.text.x = element_blank()
        )



# susceptibility plot
suscept_plot <- ggplot(long_data_s, aes(x = variable_value, y = Re, group = interaction(threshold_level, Susceptibility), color = threshold_level, linetype =  Susceptibility)) +
  geom_line(size = 1.1) +
  theme_light(base_size = 14) +
  facet_wrap(~func, nrow = 1) + 
  scale_color_manual(values = value_colors) + 
  guides(color = 'none') +
  xlab('Fold-change in transmission rate') +
  ylab('Re') +
  labs(color = NULL) +
  geom_hline(yintercept = 1, col = 'grey') +
  theme(legend.position = c(.055,.68), legend.background = element_rect(fill='transparent')) +
  # ylim(0,10) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )

# arrange plots and save , suscept_plot
multiplot <- ggarrange(functions_plot, af_plot, ncol = 1, heights = c(0.8, 1) )
ggsave(filename = '../figures/functional_forms/multiplot_new.pdf', plot = multiplot, width = 12, height = 7)

