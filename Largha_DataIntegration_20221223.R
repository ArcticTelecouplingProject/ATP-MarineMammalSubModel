# 23Dec2022
# Author: Sydney Waloven

# This dataset was built to merge collected and tidied spotted seal data sets.
# There is also a plotting section and summary section.

# List of tidied spotted seal datasets:
# 1-2_spotted_gbif_sp
# 2_tgs_seismic_2013
# 3_obis_ambon_norseman_2015
# 4_nsf_moore_mmw_2016
# 5_nsf_moore_mmw_2017
# 6_nsf_dbo_laurier_2019
# 7-1_nsf_dbo_laurier_2018
# 7-2_nsf_dbo_healy_2018
# 8_nmml_aerial_1979_2010


# Last edited: 17Apr2023

# Libraries and Packages --------------------------------------------------

library(terra)
library(sf)
library(tidyverse)
library(RColorBrewer)
library(raster)

# Clear history 
rm(list=ls())


# Import data -------------------------------------------------------------

# Import tidied data sets from Data_Processed - you will need to go through and run
# the 1-2 ARCSS_GBIF_spotted_seal script to save the outputs into your Data Processed
# folder in order to proceed with this section, etc.
# Relative paths should work here since the output should be saved locally.

# 1-2_spotted_gbif_sp
spotted_gbif_sp <- st_read("../Data_Processed/1-2_ARCSS_GBIF_spotted_seal/1-2_spotted_gbif_sp.shp")

# 2 tgs seismic 2013 - this contains data for all three species of interest
tgs_seismic_2013 <- st_read("../Data_Processed/2_obis_tgs_seismic_2013/2_obis_tgs_seismic_2013.shp")
# Isolate spotted seals from this df
spot_seismic_2013 <- subset(tgs_seismic_2013, commnNm == "Spotted Seal")

# 3 obis ambon norseman 2015
obis_ambon <- st_read("../Data_Processed/3_obis_ambon_norseman_2015/3_obis_ambon_norseman_2015.shp")
spot_ambon_2015 <- subset(obis_ambon, commnNm == "Spotted Seal")

# 4 nsf moore mmwatch 2016
mmwatch2016 <- st_read("../Data_Processed/4_nsf_moore_mmw_2016/4_nsf_moore_mmw_2016.shp")
spot_mmwatch16 <- subset(mmwatch2016, commnNm == "Spotted Seal")

# 5 nsf moore mmwatch 2017
mmwatch2017 <- st_read("../Data_Processed/5_nsf_moore_mmw_2017/5_nsf_moore_mmw_2017.shp")
spot_mmwatch17 <- subset(mmwatch2017, commnNm == "Spotted Seal")

# 6 nsf dbo laurier 2019
dbo_laurier19 <- st_read("../Data_Processed/6_nsf_dbo_laurier_2019/6_nsf_dbo_laurier_2019.shp")
spot_laurier19 <- subset(dbo_laurier19, commnNm == "Spotted Seal")

# 7-1 nsf dbo laurier 2018
dbo_laurier18 <- st_read("../Data_Processed/7-1_nsf_dbo_laurier_2018/7-1_nsf_dbo_laurier_2018.shp")
spot_laurier18 <- subset(dbo_laurier18, commnNm == "Spotted Seal")

# 7-2 nsf dbo healy 2018
dbo_healy <- st_read("../Data_Processed/7-2_nsf_dbo_healy_2018/7-2_nsf_dbo_healy_2018.shp")
spot_healy <- subset(dbo_healy, commnNm == "Spotted Seal")

# 8 nmml aerial surveys 1979-2010
nmml_aerial <- st_read("../Data_Processed/8_nmml_aerial_1979_2010/8_nmml_aerial_1979_2010.shp")
spot_aerial <- subset(nmml_aerial, commnNm == "Spotted Seal")

# Merge data sets ---------------------------------------------------------
# Need to make sure that everything is standardized like the columns, column names, and format
# of the columns

# Merge to create all_df
all_spot_df <- rbind(spotted_gbif_sp, spot_seismic_2013, spot_ambon_2015, spot_mmwatch16, spot_mmwatch17,
                     spot_laurier19, spot_laurier18, spot_healy, spot_aerial)

View(all_spot_df) # check to make sure all cols match

# Eliminate repeat occurrences
# https://statisticsglobe.com/unique-rows-of-data-frame-based-on-selected-columns-in-r
unique_spot <- distinct(all_spot_df)

# Eliminate repeat occurrences and for identifing overlaps between datasets
# unique_wal <- all_wal_df %>% dplyr::select(-source) %>% distinct

# quantifying number of occurrences after eliminating repeats
# nrow(unique_wal)

# Remove Nas
unique_spot_omit <- na.omit(unique_spot)

nrow(unique_spot_omit)

View(unique_spot_omit)

# Save this to a csv
# write.csv(st_drop_geometry(unique_spot), "../Data_Processed/largha_integrations/largha_integrated20230417.csv")


# Plotting ----------------------------------------------------------------

# Will need to use datadir for this part since it is using the DataRaw folder
datadir <- "C:/Users/Natalia Portales/Michigan State University/Waloven, Sydney - ATP-MarineMammal-DataRaw/"
datadir <- "../../ATP-MarineMammal-DataRaw/"

# Import base map - this is already projected 
base_map <- st_read(paste0(datadir, "base_map"))

# Import study area
study_area <- st_read(paste0(datadir, "study_area"))

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
  geom_sf(data = unique_spot, mapping = aes(color = year)) +
  labs(
    title = "Spotted seal occurrence in the Bering Strait Region")


# Isolate number of rows after 2010
all_spot_2010 <- unique_spot[unique_spot$year > 2009, ]

# Plotting points after 2010
ggplot() + geom_sf(data = base_crop) + 
  geom_sf(data = all_spot_2010, mapping = aes(color = year), size = 1.5) +
  labs(
    title = "Spotted seal occurrence points from 2010-2019 \n
    in the Bering Strait Region")


# Summary Stats -----------------------------------------------------------


# create new df with occurrence points and no geometry attached
all_df_drop <- unique_spot %>%
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

month_selected <- seq(6,8);
time_window = 'june-aug' ## time window 1 - june through august

month_selected <- c(9,11);
time_window = 'sep-nov' ## time window 2 - september to november

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
write.csv(sum_since, "../Output/largha_summary2_20230210.csv")



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