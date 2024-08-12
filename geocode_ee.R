# load library
library(tidyverse)

# source google api key
source('../../../R_functions/Google_API.R')

# get location names
csid <- read.csv('../CSID_scoping_review_SI_edited.csv')
csid_sm <- csid[,c('Country', 'Study.Area', 'Percentile', 'Climate_variable')]
csid_lean <- csid_sm[,c('Country', 'Study.Area', 'Climate_variable')]
csid_lean <- unique(csid_lean)
csid_lean <- subset(csid_lean, Study.Area != 'Nationwide')
csid_lean$City <- paste(csid_lean$Study.Area, csid_lean$Country, sep = ', ')

# geocode
locations_geo <- mutate_geocode(csid_lean[6:75,], City)

# combine and format
x <- csid_lean %>%
  left_join(locations_geo)

x <- x[,c('City', 'Climate_variable', 'lon', 'lat')]

# save
write.csv(x, file = '../CSID_latlon.csv', row.names = F)
