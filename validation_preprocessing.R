# outbreak locations for validation
library(tidyr)

# read locations
locations  <- read.csv('../CSID_latlon_filled.csv')
locations  <- subset(locations , Climate_variable == 'Rainfall')

# read in extreme event driven disease outbreaks
csid <- read.csv('../CSID_scoping_review_SI_edited.csv')
csid$City <- paste(csid$Study.Area, csid$Country, sep = ', ')

# Filter csid dataset to unique values and keep only relevant columns
csid_sub <- csid %>%
  filter(Climate_variable == "Rainfall") %>%
  filter(Disease %in% c("Cholera", "Diarrhea")) %>%
  left_join(locations, by = "City") %>%
  mutate(across(c(Date_start, Date_end, Date_peak),
                ~ as.Date(., format = "%m/%d/%Y"))) %>%
  filter(!if_all(c(Date_start, Date_end, Date_peak), ~ is.na(.))) %>%
  select('City', 'lon', 'lat', 'Disease', 'Date_start', 'Date_end', 'Date_peak')

# unique locations
csid_sub_unique_loc <- csid_sub %>% 
  select('City', 'lon', 'lat') %>%
  distinct(City, lon, lat, .keep_all = TRUE)

# save unique locations
write.csv(csid_sub_unique_loc, '../unique_outbreak_locations.csv')

# add ID
csid_sub_unique_loc$id <- seq(1, nrow(csid_sub_unique_loc), 1)
csid_sub <- csid_sub %>%
  left_join(csid_sub_unique_loc)

# save oubreak subet with unique location id
write.csv(csid_sub, '../outbreaks.csv', row.names = F)
