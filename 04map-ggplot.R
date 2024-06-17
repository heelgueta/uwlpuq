#syntax to plot coordinates of responses over map of punta arenas

# Install and load necessary libraries
#install.packages("ggplot2")
#install.packages("sf")
#install.packages("osmdata")
#install.packages("readr")
#install.packages("ggspatial")

library(ggplot2)
library(sf)
library(osmdata)
library(readr)
library(ggspatial)

# Read the CSV file
coordinates <- read_csv("coords.csv")

# Convert the coordinates to an sf object
coordinates_sf <- st_as_sf(coordinates, coords = c("lon", "lat"), crs = 4326)

# Print the coordinates_sf to check the data
print(coordinates_sf)

# Define the bounding box for Punta Arenas
bbox <- c(xmin = -70.955, ymin = -53.185, xmax = -70.850, ymax = -53.111)
map_data <- opq(bbox = bbox) %>%
  add_osm_feature(key = 'highway') %>%
  osmdata_sf()

# Create a data frame for the special spots
special_spots <- data.frame(
  lon = c(-70.9257, -70.8816),
  lat = c(-53.1799, -53.1151),
  name = c("Parque María Behety", "Humedal Tres Puentes")
)

# Convert the special spots to an sf object
special_spots_sf <- st_as_sf(special_spots, coords = c("lon", "lat"), crs = 4326)

# Plot the base map, regular spots, and special spots with scale bar and north arrow
ggplot() +
  geom_sf(data = map_data$osm_lines, color = "grey2") +
  geom_sf(data = coordinates_sf, aes(geometry = geometry), color = "blue2", size = 2, alpha = 0.7) +
  geom_sf(data = special_spots_sf, aes(geometry = geometry), color="green3", fill = "green3", size = 8, shape = 21, alpha = 0.7) + # Special spots
  theme_classic() 

  #the following adds scale bar and north arrow, but code is messy, depends on plot size in rstudio
  #therefore are disabled
  #+
  #annotation_scale(location = "bl", width_hint = 0.6, pad_x = unit(3.2, "in") ) + # Add scale bar
  #annotation_scale(location = "bl", width_hint = 0.5,, pad_y = unit(3, "in")) + # Center scale bar
  #annotation_north_arrow(location = "tl", which_north = "true", 
  #                       pad_x = unit(8, "in"), pad_y = unit(7, "in"),
  #                       style = north_arrow_fancy_orienteering) # Add north arrow


  