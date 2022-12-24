# 23Dec2022
# Author: Sydney Waloven

# This dataset was built to merge collected and tidied spotted seal data sets.
# There is also a plotting section and summary section.

# List of tidied spotted seal datasets:
# 1-2_spotted_gbif_sp
# 2_tgs_seismic_2013

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

# 1-2_spotted_gbif_sp (SpatialPointsDataFrame of spotted_arcss_gbif)
spotted_gbif_sp <- st_read("../Data_Processed/1-2_spotted_gbif_sp.shp")

# 2 tgs seismic 2013 - this contains data for all three species of interest
tgs_seismic_2013 <- st_read("../Data_Processed/2_tgs_seismic_2013.shp")
## Isolate out spotted seals from this df
spot_seismic_2013 <- subset(tgs_seismic_2013, commnNm == "Spotted Seal")

# Concatenate data sets
# all_marmams <- c(walrus_arcss_gbif, spotted_arcss_gbif, bearded_arcss_gbif, tgs_seismic_2013)
# this creates a matrix instead of joining the columns appropriately

# Merge data sets ---------------------------------------------------------
# Need to make sure that everything is standardized like the columns, column names, and format
# of the columns

# Merge to create all_df
all_spot_df <- rbind(spotted_gbif_sp, spot_seismic_2013)

View(all_spot_df) # check to make sure all cols match

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


# Plot all spotted seal occurrence points against uncropped base map
ggplot() + geom_sf(data = base_crop) + 
  geom_sf(data = all_spot_df, mapping = aes(color = year)) +
  labs(
    title = "Spotted seal occurrence in the Bering Strait Region")


# Isolate number of rows after 2010
all_spot_2010 <- all_spot_df[all_spot_df$year > 2009, ]

# Plotting points after 2010
ggplot() + geom_sf(data = base_crop) + 
  geom_sf(data = all_spot_2010, mapping = aes(color = year), size = 1.5) +
  labs(
    title = "Spotted seal occurrence points from 2010-2019 \n
    in the Bering Strait Region")


# Summary Stats -----------------------------------------------------------


# create new df with occurrence points and no geometry attached
all_df_drop <- all_spot_df %>%
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

# month_selected <- seq(6,8);
# time_window = 'time window 1' ## time window 1 - june through august

month_selected <- c(9,11);
time_window = 'time window 2' ## time window 2 - september to november

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
write.csv(sum_since, "../Output/spotted_int_window2_20221223.csv")



# Graveyard ---------------------------------------------------------------

# change country colors with scale_fill_manual()
# scale_fill_manual(values = c("white", "red", "blue"))

# Original time windows for summary statistics loop
## choose one of the time windows for the loop
# month_selected <- seq(1,12);
# time_window = 'year round' ## year round

# month_selected <- seq(6,10);
# time_window = 'summer' ## summer - june through october

# month_selected <- c(11, 12, seq(1,5));
# time_window = 'winter' ## winter - november through may