# Title: Marine Mammal Data Paper Tidying 
# Author: Natalia Portales and Sydney Waloven
# Date: 2024-01-20
# Description: Source: USGS. This dataset was obtained from Behavior of Pacific Walruses Hauled Out on Sea Ice during UAS Overflights, Eastern Chukchi Sea 2015
# https://alaska.usgs.gov/products/data.php?dataid=74

# This script follows standardized methods of tidying for the marine mammal data sets we have collected contributing to a data paper.

# setup -------------------------------------------------------------------

# Load libraries
library(sf)
library(tidyverse)

# Clear environment history 
rm(list=ls())

# Import data
# data_set <- read_csv("pathway/to/data/set.csv")

# This example will use the datadir method that we used as a workaround for the file path issue of the raw data.
datadir <- "C:/Users/Natalia Portales/Michigan State University/Waloven, Sydney - ATP-MarineMammal-DataRaw/"
datadir <- "../../ATP-MarineMammal-DataRaw/"

# This script will tidy USDA UAS overflight surveys from 2015.
usgs_walrus_uas <- read.csv(paste0(datadir,"10_usgs_walrus_behavior_uas_2015/walrus_environConditionsUAS_chukchi_battaile_2015.csv"))


# data tidying ------------------------------------------------------------

# Check out column names
names(usgs_walrus_uas)

usgs_walrus <- usgs_walrus_uas %>%
  
  # Drop NAs in coordinates
  drop_na(Latitude, Longitude) %>%
  
  # Separate DateTimeStart into month and day cols by using substr()
  mutate(year = 2015,
         month = substr(Flight, 5, 6),
         day = substr(Flight, 7, 8),
         
         # Add columns for scientificName and commonName
         scientificName = "Odobenus rosmarus divergens",
         
         # Add method of observation and source columns
         methodObs = "Research vessel visual observation",
         source = "USGS Walrus Behavior UAS 2015") %>% 
  
  # Remove unneeded columns and rename
  # Order of columns here determines order of columns in df. Can also rename columns in this function (new name on left, old name on right).
  select(scientificName,
         year,
         month,
         day,
         decimalLat = Latitude,
         decimalLon = Longitude,
         count = Group_Size,
         methodObs,
         source) %>% 
  
  # Convert to sf and transform to Alaska Albers
  st_as_sf(coords = c("decimalLon", "decimalLat"),
           crs = 4326,
           remove = FALSE) %>% 
  st_transform(3338) %>% 
  
  # Eliminate repeat occurrences
  distinct() %>% 
  
  # Rearrange - this will order the data by the earliest date and then follow chronologically
  arrange(year, month, day)

# Save as csv into respective folder - add tidy to the end of file names so we can differentiate easily
write_csv(usgs_walrus, "../Data_Processed/10_usgs_walrus_behavior_uas_2015/10_usgs_walrus_behavior_uas_tidy.csv")

# Read in tidied data
# usgs_walrus <- read_csv("../Data_Processed/10_tidy_usgs_walrus_behavior_uas_2015.csv")
