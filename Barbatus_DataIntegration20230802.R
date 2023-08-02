# 09Jun2023
# Author: Sydney Waloven

# This dataset was built to merge collected and tidied bearded seal data sets.
# There is also a plotting section and summary section. This script builds off
# the previous Data Integration script: Barbatus_DataIntegration20230109.

# List of tidied bearded seal datasets:
# 1-3_bearded_gbif_sp
# 2_tgs_seismic_2013
# 3_obis_ambon_norseman_2015
# 4_nsf_moore_mmw_2016
# 5_nsf_moore_mmw_2017
# 6_nsf_dbo_laurier_2019
# 7-1_nsf_dbo_laurier_2018
# 7-2_nsf_dbo_healy_2018
# 8 nmml aerial 1979-2010
# 1-3-1_gbif_beard

# Last edited: 02Aug2023


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
# the 1-3_arcss_gbif_bearded_seal script to save the outputs into your Data Processed
# folder in order to proceed with this section, etc.

# 1-3 bearded_gbif_sp
bearded_gbif_sp <- st_read("../Data_Processed/1-3_ARCSS_GBIF_bearded_seal/1-3_bearded_gbif_sp.shp")

# 2 tgs seismic 2013 - this contains data for all three species of interest
tgs_seismic_2013 <- st_read("../Data_Processed/2_obis_tgs_seismic_2013/2_obis_tgs_seismic_2013.shp")
# Isolate bearded seals from this df
beard_seismic <- subset(tgs_seismic_2013, commnNm == "Bearded Seal")

# 3 obis ambon norseman 2015
obis_ambon <- st_read("../Data_Processed/3_obis_ambon_norseman_2015/3_obis_ambon_norseman_2015.shp")
beard_ambon <- subset(obis_ambon, commnNm == "Bearded Seal")

# 4 nsf moore mmwatch 2016
mmwatch2016 <- st_read("../Data_Processed/4_nsf_moore_mmw_2016/4_nsf_moore_mmw_2016.shp")
beard_mmwatch16 <- subset(mmwatch2016, commnNm == "Bearded Seal")

# 5 nsf moore mmwatch 2017
mmwatch2017 <- st_read("../Data_Processed/5_nsf_moore_mmw_2017/5_nsf_moore_mmw_2017.shp")
beard_mmwatch17 <- subset(mmwatch2017, commnNm == "Bearded Seal")

# 6 nsf dbo laurier 2019
dbo_laurier19 <- st_read("../Data_Processed/6_nsf_dbo_laurier_2019/6_nsf_dbo_laurier_2019.shp")
beard_laurier19 <- subset(dbo_laurier19, commnNm == "Bearded Seal")

# 7-1 nsf dbo laurier 2018
dbo_laurier18 <- st_read("../Data_Processed/7-1_nsf_dbo_laurier_2018/7-1_nsf_dbo_laurier_2018.shp")
beard_laurier18 <- subset(dbo_laurier18, commnNm == "Bearded Seal")

# 7-2 nsf dbo healy 2018
dbo_healy <- st_read("../Data_Processed/7-2_nsf_dbo_healy_2018/7-2_nsf_dbo_healy_2018.shp")
beard_healy <- subset(dbo_healy, commnNm == "Bearded Seal")

# 8 nmml aerial survey 1979-2010
nmml_aerial <- st_read("../Data_Processed/8_nmml_aerial_1979_2010/8_nmml_aerial_1979_2010.shp")
beard_nmml <- subset(nmml_aerial, commnNm == "Bearded Seal")

# 1-3-1_gbif_beard
gbif_beard <- st_read("../Data_Processed/1-3-1_gbif_beard/1-3-1_gbif_beard.shp")

# 13 aerial surveys asamm
asamm <- st_read("../Data_Processed/13_aerial_surveys_asamm/13_aerial_surveys_asamm.shp")
asamm_beard <- subset(asamm, commnNm == "Bearded Seal")

# Merge data sets ---------------------------------------------------------
# Need to make sure that everything is standardized like the columns, column names, and format
# of the columns

# Merge to create all_df
all_beard_df <- rbind(bearded_gbif_sp, beard_seismic, beard_ambon, beard_mmwatch16, beard_mmwatch17,
                      beard_laurier19, beard_laurier18, beard_healy, beard_nmml, gbif_beard, asamm_beard)


# Eliminate other subspecies and outlier occurrences ----------------------

lat_min = 45
lat_max = 90
lon_min = -170
lon_max = -140

# Convert lat/lon columns to numeric
all_beard_df$decmlLt <- as.numeric(all_beard_df$decmlLt)
all_beard_df$decmlLn <- as.numeric(all_beard_df$decmlLn)

pacific_beard <- all_beard_df %>%
  filter(scntfcN %in% c("Erignathus barbatus", "Erignathus barbatus nauticus") &
        # latitude should be greater than or equal to lat_min and less than or equal to lat_max
        decmlLt >= lat_min & decmlLt <= lat_max,
        # longitude should be greater than or equal to lon_min and less than or equal to lon_max
        decmlLn >= lon_min & decmlLn <= lon_max)

# Eliminate repeats -------------------------------------------------------

# Eliminate repeat occurrences to ensure they were handled properly in each individual script
unique_beard <- distinct(pacific_beard)

# Number of occurrences after eliminating repeats
# nrow(unique_beard)

# Remove NAs
unique_beard_omit <- na.omit(unique_beard)

nrow(unique_beard_omit)


# Save variables ----------------------------------------------------------

# Save this to a csv in a folder called barbatus_integrations
write.csv(st_drop_geometry(unique_beard_omit), "../Data_Processed/barbatus_integrations/barbatus_integrated20230802.csv")

# Save to shp
st_write(unique_beard_omit, "../Data_Processed/barbatus_integrations/barbatus_integrated20230802.shp")

### SKIP TO THIS LINE TO AVOID HAVING TO RUN THE ABOVE CODE TO IMPORT THE
### INTEGRATED DATASET INTO THE ENVIRONMENT
ugruk <- st_read("../Data_Processed/barbatus_integrations/barbatus_integrated20230802.shp")
# Inupiaq word for bearded seal

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


# Plot all bearded seal occurrence points against cropped base map
ggplot() + geom_sf(data = base_crop) + 
  geom_sf(data = pacific_beard, mapping = aes(color = year)) +
  labs(
    title = "Bearded seal occurrences in the Bering Strait Region")


# Isolate number of rows after 2010
ugruk2010 <- ugruk[ugruk$year > 2009, ]

# Plotting points after 2010
ggplot() + geom_sf(data = base_crop) + 
  geom_sf(data = ugruk2010, mapping = aes(color = year), size = 1.5) +
  labs(
    title = "Bearded seal occurrences from 2010-2019 \n
    in the Bering Strait Region")


# Summary Stats -----------------------------------------------------------


# create new df with occurrence points and no geometry attached
all_df_drop <- ugruk %>%
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
write.csv(sum_since, "../Output/prelim_models/summer/barbatus_summer20230802.csv")

