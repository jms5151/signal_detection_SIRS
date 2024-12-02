# Load the ggplot2 package
library(ggplot2)
library(ggrepel)

# Generate some example data
df <- read.csv('../data/Climate-Disease_Detection_Scoring_Table.csv')

# Create the scatterplot with labels
p <- ggplot(df, aes(x = Climate_disease_link_rating, y = Attribution_rating, fill = Disease_type)) + #Natural_Immunity_Duration
  geom_abline(slope = 1, intercept = 0, col = 'grey', linetype = 'dashed') +
  geom_point(shape = 21, color = 'black', size = 8, stroke = 0.5) +
  geom_text_repel(aes(label = Disease), 
                  box.padding = 2,   # Adjust padding around the text
                  point.padding = 1.5,   # Adjust space around points
                  force = 1,           # Increase the force of repulsion
                  max.iter = 1000,     # Increase the number of iterations for overlap removal
                  color = 'black', 
                  size = 3
  ) +
  theme_bw() + 
  theme(axis.text=element_blank()
        , axis.ticks=element_blank()) + 
  labs(x = 'How well we understand climate-disease link'
       , y = 'Ability to detect climate influence on an outbreak'
       , title = 'Relative confidence in climate attribution of disease outbreaks') + #natural
  xlim(1,6) +
  ylim(1,6) +
  scale_fill_discrete(name = 'Disease type')

ggsave(filename = '../figures/scatterplot_guide.pdf', plot = p, width = 8, height = 4) # natural
