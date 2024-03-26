# Load the ggplot2 package
library(ggplot2)
library(ggrepel)

# Generate some example data
df <- read.csv('../data/scatter.csv')

# Create the scatterplot with labels
ggplot(df, aes(x = df$Incubation_Period, y = df$Vaccine_Induced_Immunity_Duration)) + #Natural_Immunity_Duration
  geom_point(shape = 21, color = "black", fill = "#008080AA", size = 5, stroke = 0.5) +
  # geom_text(aes(label = Disease), vjust = -1, hjust = 0.5, color = "black", size = 3) + 
  geom_text_repel(aes(label = Disease), 
                  box.padding = 0.5,   # Adjust padding around the text
                  point.padding = 0.5, # Adjust space around points
                  force = 1,           # Increase the force of repulsion
                  max.iter = 2000,     # Increase the number of iterations for overlap removal
                  color = "black", 
                  size = 3
  ) +
  theme_minimal() + 
  # ggtitle("Scatterplot with Transparent Teal Points and Labels") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  labs(x = 'Incubation period (days)', y = 'Duration of vaccine-induced immunity (years)') #natural

ggsave('../figures/scatterplot_vaccine_immunity.pdf') # natural
