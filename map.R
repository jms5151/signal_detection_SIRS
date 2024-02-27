# Load required libraries
library(maps)
library(mapdata)

# Define the world map
world_map <- map("world", plot = FALSE)

# Define the list of cities and their corresponding colors
cities_to_color <- list(
  Blue = list(
    "Addis Ababa, Ethiopia\n (Cfb)" = c(38.7468, 9.0227),
    "Port-au-Prince, Haiti\n (Aw)" = c(-72.3345, 18.5944),
    "Khartoum, Sudan\n (BSh)" = c(32.5599, 15.5007),
    "Jammu, India\n (Dfb)" = c(74.8570, 32.7266),
    "Xinjiang, China\n (BWk)" = c(85.7221, 40.8429)
  ),
  Red = list(
    "Ouagadougou, Burkina Faso\n (BSh)" = c(-1.5332, 12.3605),
    "Rio de Janeiro, Brazil\n (Aw)" = c(-43.1729, -22.9068),
    "Karachi, Pakistan\n (BWh)" = c(67.0099, 24.8615),
    "Veneto, Italy\n (Cfa)" = c(11.6576, 45.4039),
    "Manila, Philippines\n (Aw)" = c(120.9842, 14.5995),
    "Suva, Fiji\n (Af)" = c(178.4419, -18.1248)
  )
)

# Initialize the plot with all countries in white with black borders
plot(world_map, col = "white", bg = "lightgray", xlim = c(-180, 180), ylim = c(-90, 90),
     xlab = "", ylab = "")

# Add borders to all countries
map("world", interior = TRUE, add = TRUE, col = "darkgrey", lwd = 0.5)

# Plot blue cities
for (city in names(cities_to_color$Blue)) {
  city_coords <- cities_to_color$Blue[[city]]
  points(city_coords[1], city_coords[2], bg = "lightblue", pch = 21, cex = 2)
  text(city_coords[1], city_coords[2], labels = city, pos = 1, cex = 0.9)
}

# Plot red cities
for (city in names(cities_to_color$Red)) {
  city_coords <- cities_to_color$Red[[city]]
  points(city_coords[1], city_coords[2], bg = "orange", pch = 21, cex = 2)
  text(city_coords[1], city_coords[2], labels = city, pos = 1, cex = 0.9)
}

# Add a legend
legend("topleft", legend = c("water-borne disease", "vector-borne disease"), fill = c("lightblue", "orange"), cex = 1.1, bty = 'n')
