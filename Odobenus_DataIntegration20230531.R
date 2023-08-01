# 31May2023
# Author: Sydney Waloven and Natalia Portales

# This dataset was built to merge collected and Pacific walrus data sets.
# This builds off of the previous Odobenus_DataIntegration_20230116 script.
# There is also a plotting section and summary section.

# List of tidied walrus datasets:
# 1-1_walrus_gbif_sp
# 2_tgs_seismic_2013
# 3_obis_ambon_norseman_2015
# 4_nsf_moore_mmw_2016
# 5_nsf_moore_mmw_2017
# 6_nsf_dbo_laurier_2019
# 7-1_nsf_dbo_laurier_2018
# 7-2_nsf_dbo_healy_2018
# 8_nmml_aerial_1979_2010
# 9_usgs_walrus_sex_age_comp_2013_2015
# 10_usgs_walrus_behavior_uas_2015
# 1-1-1_gbif_walrus

# Last edited: 03Jul2023


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
# the 1-1_ARCSS_GBIF_walrus script to save the outputs into your Data Processed
# folder in order to proceed with this section, etc.
# Relative paths should work here since the output should be saved locally.

# 1-1_walrus_gbif_sp
walrus_gbif_sp <- st_read("../Data_Processed/1-1_ARCSS_GBIF_walrus/1-1_walrus_gbif_sp.shp")

# 2 tgs seismic 2013 - this contains data for all three species of interest
tgs_seismic_2013 <- st_read("../Data_Processed/2_obis_tgs_seismic_2013/2_obis_tgs_seismic_2013.shp")
# Isolate walruses from this df
wal_seismic_2013 <- subset(tgs_seismic_2013, commnNm == "Walrus")

# 3 obis ambon norseman 2015
obis_ambon <- st_read("../Data_Processed/3_obis_ambon_norseman_2015/3_obis_ambon_norseman_2015.shp")
wal_ambon_2015 <- subset(obis_ambon, commnNm == "Walrus")

# 4 nsf moore mmwatch 2016
mmwatch2016 <- st_read("../Data_Processed/4_nsf_moore_mmw_2016/4_nsf_moore_mmw_2016.shp")
wal_mmwatch16 <- subset(mmwatch2016, commnNm == "Walrus")

# 5 nsf moore mmwatch 2017
mmwatch2017 <- st_read("../Data_Processed/5_nsf_moore_mmw_2017/5_nsf_moore_mmw_2017.shp")
wal_mmwatch17 <- subset(mmwatch2017, commnNm == "Walrus")

# 6 nsf dbo laurier 2019
dbo_laurier19 <- st_read("../Data_Processed/6_nsf_dbo_laurier_2019/6_nsf_dbo_laurier_2019.shp")
wal_laurier19 <- subset(dbo_laurier19, commnNm == "Walrus")

# 7-1 nsf dbo laurier 2018
dbo_laurier18 <- st_read("../Data_Processed/7-1_nsf_dbo_laurier_2018/7-1_nsf_dbo_laurier_2018.shp")
wal_laurier18 <- subset(dbo_laurier18, commnNm == "Walrus")

# 7-2 nsf dbo healy 2018
dbo_healy <- st_read("../Data_Processed/7-2_nsf_dbo_healy_2018/7-2_nsf_dbo_healy_2018.shp")
wal_healy <- subset(dbo_healy, commnNm == "Walrus")

# 8 nmml aerial 1979-2010
nmml_aerial <- st_read("../Data_Processed/8_nmml_aerial_1979_2010/8_nmml_aerial_1979_2010.shp")
wal_nmml <- subset(nmml_aerial, commnNm == "Walrus")

# 9 usgs walrus sex age comp 2013-2015
usgs_comp <- st_read("../Data_Processed/9_usgs_walrus_sex_age_comp_2013_2015/9_usgs_walrus_sex_age_comp_2013_2015.shp")
wal_comp <- subset(usgs_comp, commnNm == "Walrus")

# 10 usgs walrus behavior uas 2015
usgs_behavior <- st_read("../Data_Processed/10_usgs_walrus_behavior_uas_2015/10_usgs_walrus_behavior_uas_2015.shp")
wal_behavior <- subset(usgs_behavior, commnNm == "Walrus")

# 1-1-1_gbif_walrus
gbif_walrus <- st_read("../Data_Processed/1-1-1_gbif_walrus/1-1-1_gbif_walrus.shp")

# 13 aerial surveys asamm
asamm <- st_read("../Data_Processed/13_aerial_surveys_asamm/13_aerial_surveys_asamm.shp")
asamm_walrus <- subset(asamm, commnNm == "Walrus")


# Merge data sets ---------------------------------------------------------
# Need to make sure that everything is standardized like the columns, column names, and format
# of the columns

# Merge to create all_df
all_wal_df <- rbind(walrus_gbif_sp, wal_seismic_2013, wal_ambon_2015, wal_mmwatch16, wal_mmwatch17,
                    wal_laurier19, wal_laurier18, wal_healy, wal_nmml, wal_comp, wal_behavior,
                    gbif_walrus, asamm_walrus)
# Check to make sure all columns match

# Number of occurrences before eliminating repeats
# nrow(all_wal_df)

# Eliminate repeat occurrences
unique_wal <- distinct(all_wal_df)

# Remove NAs
unique_wal_omit <- na.omit(unique_wal)

nrow(unique_wal_omit)

# Save this to a csv
# write.csv(st_drop_geometry(unique_wal_omit), "../Data_Processed/odobenus_integrations/odobenus_integrated20230801.csv")

# Save to shp
# st_write(unique_wal_omit, "../Data_Processed/odobenus_integrations/odobenus_integrated20230801.shp")

### SKIP TO THIS LINE TO AVOID HAVING TO RUN THE ABOVE CODE TO IMPORT THE
### INTEGRATED DATASET INTO THE ENVIRONMENT
all_walrus <- st_read("../Data_Processed/odobenus_integrations/odobenus_integrated20230801.shp")


# Plotting ----------------------------------------------------------------

# Will need to use datadir for this part since it is using the DataRaw folder
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


# Plot all Walrus occurrence points against uncropped base map
ggplot() + geom_sf(data = base_crop) + 
  geom_sf(data = unique_wal_omit, mapping = aes(color = year)) +
  labs(
    title = "Walrus occurrences in the Bering Strait Region")


# Isolate number of rows after 2010
unique2010 <- unique_wal_omit[unique_wal_omit$year > 2009, ]

# Save to csv
write.csv(st_drop_geometry(unique2010), "../Data_Processed/Odobenus_DataIntegration_20230116/odobenus2010_integrated20230210.csv")

# Plotting points after 2010Odobenus_DataIntegration_20230116
ggplot() + geom_sf(data = base_crop) + 
  geom_sf(data = unique2010, mapping = aes(color = year), size = 1.5) +
  labs(
    title = "Walrus occurrences from 2010-2019 \n
    in the Bering Strait Region")


# Summary Stats -----------------------------------------------------------


# create new df with occurrence points and no geometry attached
all_df_drop <- all_walrus %>%
  st_drop_geometry() %>%
  group_by(year, month, day) %>%
  dplyr::summarize(n=n()) %>%
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
month_selected <- seq(1,12);
time_window = 'year round' ## year round

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
write.csv(sum_since, "../Output/prelim_models/summer/odobenus_summer20230801.csv")


## For Natalia's Mapping ---------------------------------------------------------------------------------------------

# Save integrated dataset as csv and shp for mapping entire dataset
# st_write(unique_wal_omit, "../Data_Processed/Odobenus_DataIntegration_20230116/mapping_odobenus_integrated20230210.shp")

# Isolate number of rows after 2010
# unique2010 <- unique_wal_omit[unique_wal_omit$year > 2009, ]

# class(unique2010$year)
# unique2010$year <- as.numeric(unique2010$year)

## Choose only one of the time windows
# include time window of interest: june-aug
# june_unique2010 <- unique2010[unique2010$month %in% c("6", "7", "8"), ]

# include time window of interest: june-aug
# sep_unique2010 <- unique2010[unique2010$month %in% c("9", "10", "11"), ]

# View(june_unique2010)
# View(sep_unique2010)

# Save as shp and csv june-aug integrated dataset after 2010
# st_write(june_unique2010, "../Data_Processed/Odobenus_DataIntegration_20230116/Summer/june_mapping_odobenus_integrated20230210.shp")

# Save as shp and csv june-aug integrated dataset after 2010
# st_write(sep_unique2010, "../Data_Processed/Odobenus_DataIntegration_20230116/Fall/sep_mapping_odobenus_integrated20230210.shp")


