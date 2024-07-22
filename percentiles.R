lowe_data <- read.csv('../Lowe_etal_data_2000_2019.csv')

distribution <- ecdf(lowe_data$pdsi)
distribution(-4)
distribution(4)


