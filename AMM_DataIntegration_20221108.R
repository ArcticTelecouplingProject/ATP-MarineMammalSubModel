# 08Nov2022
# Author: Sydney Waloven

# This dataset was built to merge collected and tidied AMMs data sets. There is also
# a plotting section and summary section.

# List of tidied AMMs data sets:
# 1-1 walrus arcss gbif
# 1-2 spotted arcss gbif
# 1-3 bearded arcss gbif
# 2 tgs seismic 2013

# Libraries and Packages --------------------------------------------------

library(terra)
library(sf)
library(tidyverse)
library(RColorBrewer)
library(raster)

# Clear history 
rm(list=ls())


# Import data -------------------------------------------------------------

# Import tidied data sets from Data_Processed

# 1-1 walrus arcss gbif
walrus_arcss_gbif <- read.csv("../Data_Processed/1-1_walrus_gbif_sp.csv")

# 1-2 spotted arcss gbif
spotted_arcss_gbif <- read.csv("../Data_Processed/1-2_spotted_gbif_sp.csv")

# 1-3 bearded arcss gbif
bearded_arcss_gbif <- read.csv("../Data_Processed/1-3_bearded_gbif_sp.csv")

# 2 tgs seismic 2013
tgs_seismic_2013 <- read.csv("../Data_Processed/2_tgs_seismic_2013.csv")


# Concatenate data sets
# all_marmams <- c(walrus_arcss_gbif, spotted_arcss_gbif, bearded_arcss_gbif, tgs_seismic_2013)
# this creates a matrix instead of joining the columns appropriately

# Merge data sets ---------------------------------------------------------
# Need to make sure that everything is standardized like the columns, column names, and format
# of the columns

# Merge to create all_df_drop
all_df <- rbind(walrus_arcss_gbif, spotted_arcss_gbif, bearded_arcss_gbif, tgs_seismic_2013)
# Successfully merged S21W09Nov2022
# How come the CRS disappeared?

# Convert data back to a spatial object
all_df_sp <- st_as_sf(all_df, coords = c("decimalLon", "decimalLat"), crs = 4326)

# Transform to Alaska Albers projection
all_df_sp <- all_df_sp %>% st_transform(crs = 3338)
# Chcek CRS
st_crs(all_df_sp)
# No more lat/lon cols again - is this okay?


# Plotting ----------------------------------------------------------------


# Import base map - this is already projected 
base_map <- st_read("../../ATP-MarineMammal-DataRaw/base_map")

# Import study area
study_area <- st_read("../../ATP-MarineMammal-DataRaw/study_area")

# Project study area to 3338
study_area <- study_area %>% st_transform(3338)

# https://gis.stackexchange.com/questions/230900/crop-simple-features-object-in-r
# Read xmin, ymin, xmax, and ymax values with st_bbox()
# study_bbox <- st_bbox(study_area)

# Crop base map to just Bering Strait Region area (based on study_bbox values)
# base_crop <- st_crop(base_map, c(xmin = -1100000, ymin = 1410000, xmax = -450000, ymax = 1960000))
# Crop base map based on decreased base_map bbox values
base_crop <- st_crop(base_map, c(xmin = -2173499, ymin = 159766, xmax = 1661246, ymax = 3000000))

# Plot all AMM occurrence points against uncropped base map
ggplot() + geom_sf(data = base_map) + 
  geom_sf(data = all_df_sp, mapping = aes(color = year)) +
  labs(
    title = "Arctic marine mammal occurrence in the Bering Strait Region")

# Isolate number of rows after 2015
all_df_2015 <- all_df_sp[all_df_sp$year > 2014, ]

# Plotting points after 2015
ggplot() + geom_sf(data = base_map) + 
  geom_sf(data = all_df_2015, mapping = aes(color = year)) +
  labs(
    title = "Arctic marine mammal occurrence points from 2016-2019 \n
    in the Bering Strait Region")


# Pacific walrus plotting -------------------------------------------------

# Subset Pacific walrus points out of all_df
all_walrus <- all_df_sp[all_df_sp$commonName %in% "Walrus", ]

# Subset walrus points after the year 2010
walrus2010 <- all_walrus[all_walrus$year > 2009, ]

# Plotting all Pacific walrus points in all_df
ggplot() + geom_sf(data = base_crop) +
  geom_sf(data = all_walrus, mapping = aes(color = year)) +
  labs(
    title = "Pacific walrus occurrence points in the Bering Strait Region")

# Plotting points after 2010 (with cropped base)
ggplot() + geom_sf(data = base_crop) +
  geom_sf(data = walrus2010, mapping = aes(color = year), size = 1.5) +
  labs(
    title = "Pacific walrus occurrence points from 2012-2019 \n
    in the Bering Strait Region")
# need to figure out how to add a counter to differentiate how many pts are in an area


# Spotted seal plotting ---------------------------------------------------

# Subset Pacific walrus points out of all_df
all_spotted <- all_df_sp[all_df_sp$commonName %in% "Spotted seal", ]

# Subset walrus points after the year 2010
spotted2010 <- all_spotted[all_spotted$year > 2009, ]

# Plotting all Pacific walrus points in all_df
ggplot() + geom_sf(data = base_crop) +
  geom_sf(data = all_spotted, mapping = aes(color = year)) +
  labs(
    title = "Spotted seal occurrence points in the Bering Strait Region")

# Plotting points after 2010 (with cropped base)
ggplot() + geom_sf(data = base_crop) +
  geom_sf(data = spotted2010, mapping = aes(color = year), size = 1.5) +
  labs(
    title = "Spotted seal occurrence points from 2010-2019 \n
    in the Bering Strait Region")
# need to figure out how to add a counter to differentiate how many pts are in an area
# also how to exclude NA?


# Bearded seal plotting ---------------------------------------------------

# Subset bearded seal points out of all_df
all_bearded <- all_df_sp[all_df_sp$commonName %in% "Bearded seal", ]

# Subset walrus points after the year 2010
bearded2010 <- all_bearded[all_bearded$year > 2009, ]

# Plotting all Pacific walrus points in all_df
ggplot() + geom_sf(data = base_crop) +
  geom_sf(data = all_bearded, mapping = aes(color = year)) +
  labs(
    title = "Bearded seal occurrence points in the Bering Strait Region")

# Plotting points after 2010 (with cropped base)
ggplot() + geom_sf(data = base_crop) +
  geom_sf(data = spotted2010, mapping = aes(color = year), size = 1.5) +
  labs(
    title = "Bearded seal occurrence points from 2010-2019 \n
    in the Bering Strait Region")
# need to figure out how to add a counter to differentiate how many pts are in an area
# also how to exclude NA?


# Summary Stats -----------------------------------------------------------


# create new df with occurrence points and no geometry attached
all_df_drop <- all_df_sp %>%
  st_drop_geometry() %>%
  group_by(year, month, day) %>%
  summarize(n=n()) %>%
  ungroup()

# check classes to make sure year and month cols are numeric
all_df_drop$year <- as.numeric(all_df_drop$year)
all_df_drop$month <- as.numeric(all_df_drop$month) 
all_df_drop$day <- as.numeric(all_df_drop$day)

class(all_df_drop$year)
class(all_df_drop$month)
class(all_df_drop$day)

## For loop to avoid rewriting code each time to look at the sum of pts since a year

# number of months per year
n.months <- n_distinct(all_df_drop$month)
# unique years in data set, in order from earliest to latest
my.years <- sort(unique(all_df_drop$year))
# number of years
# n.years <- n_distinct(all_df_drop$year) ## with NA
n.years <- n_distinct(my.years)
# my.sums will contain each of the cumulative sums across the years
my.sums <- rep(NA, n.years)

## choose one of the time windows for the loop
# month_selected <- seq(1,12);
# time_window = 'year round' ## year round

# month_selected <- seq(6,10);
# time_window = 'summer' ## summer - june through october
 
month_selected <- c(11, 12, seq(1,5));
time_window = 'winter' ## winter - november through may

sum_since <- data.frame()
for (i in 1:n.years) {
  my.sums[i] <- all_df_drop %>%
    dplyr::filter(year >= my.years[i]) %>%
    dplyr::filter(month %in% month_selected) %>%
    dplyr::select(n) %>%
    sum(na.rm = TRUE)
  
  # print out the result for easy checking
  cat(my.years[i], ':', my.sums[i], '\n')
  
  # put the results into a dataframe
  sum_i <- cbind(year = my.years[i], sum = my.sums[i], time_window = time_window)
  sum_since <- rbind(sum_since, sum_i)
}

# save summary to a csv
# write.csv(sum_since, "../ATP_Data_Integration/Output/AMM_DataIntegration_20221108_winter.csv")



# Graveyard ---------------------------------------------------------------

# change country colors with scale_fill_manual()
# scale_fill_manual(values = c("white", "red", "blue"))
