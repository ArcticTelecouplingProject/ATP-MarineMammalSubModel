# https://stackoverflow.com/questions/51718839/in-r-how-do-i-run-st-convex-hull-function-on-point-sf-object

# Example
library(tmap)
library(sf)
nc <- st_centroid(st_read(system.file("shape/nc.shp", package="sf")))
qtm(nc)

ch <- st_convex_hull(st_union(nc)) 
qtm(ch)

# -----

# Goal: Create a polygon based on the minimum convex hull surrounding the occurrence data for the three species of interest

library(tmap)
library(sf)

data <- st_read("../Data_Processed/marine_mammal_integration/final_marine_mammal_integration.shp")

nc <- st_centroid(data)
qtm(nc)

# Creating convex hull surrounding the occurrences
ch <- st_convex_hull(st_union(nc)) 
qtm(ch)

# Save data
st_write(ch, "../Data_Processed/marine_mammal_integration/convex_hull_occurrences.shp", append = FALSE)
