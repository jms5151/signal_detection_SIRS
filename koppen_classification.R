# load libraries
# library(tidyverse)

# read files
ee_points <- read.csv('../CSID_latlon_filled.csv')
ee_koppen <- read.csv('../Koppen_Geiger_Points_From_CSV.csv')
koppen_key <- read.csv('../Koppen_Key.csv')

# join data
ee_points$Koppen_Number <- ee_koppen$Koppen 
ee_df <- merge(ee_points, koppen_key, all.x = TRUE)

# save
write.csv(ee_df, '../EE_Koppen.csv', row.names = F)
