# Title: Marine Mammal Data Paper Tidying 
# Author: Sydney Waloven
# Date: 2024-11-19
# Description: Marine Bird Sighting Data, Arctic Marine Biodiversity Observing Network (AMBON) Chukchi Sea research cruise on the vessel Norseman II. https://www.gbif.org/dataset/d021ed3a-7179-4f25-bfd3-90d72cc6fd38

# This script follows a standardized method for data tidying discussed in our data paper.

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

# Read in data
gbif <- st_read(paste0(datadir, "03_obis_ambon_norseman_2015/0022775-241107131044228.csv"))

# data tidying ------------------------------------------------------------

# Check out column names
names(gbif)

# Replace empty strings and missing cell values with true NAs
na_gbif <- gbif %>% 
  mutate(
    across(
      everything(),
      ~na_if(., "")),
    across(
      everything(),
      ~na_if(., "missing"))) %>% 
  mutate(year = ifelse(year == 0, NA, year)) 

gbif_new <- na_gbif %>%
  
  # Drop NAs in coordinate, scientific name, and date columns
  drop_na(decimalLatitude, decimalLongitude, species, year) %>% 
  
  # Modify and create new columns using mutate() function
  # Add method of observation and source columns
  mutate(methodObs = "Line transect visual survey",
         source = "Marine Birds Norseman II Cruise 2015",
         
         # Convert appropriate columns to numeric
         decimalLatitude = as.numeric(decimalLatitude),
         decimalLongitude = as.numeric(decimalLongitude),
         year = as.numeric(year),
         month = as.numeric(month),
         day = as.numeric(day),
         individualCount = as.numeric(individualCount),
         
         # Populate count column with 1 observation per row
         count = 1,
         
         # Add column to locate where the data was downloaded from
         dataOrigin = "GBIF") %>% 
  
  # Filter to select our species of interest and occurrences within our study bounds
  filter(species %in% c("Erignathus barbatus", "Phoca largha", "Odobenus rosmarus"),
         decimalLatitude >= 46.08836 & decimalLatitude <= 73.92759, 
         decimalLongitude >= 172.18938 | decimalLongitude <= -146.30217) %>%
  
  # Remove unneeded columns and rename
  select(gbifID,
         scientificName = species,
         year,
         month,
         day,
         decimalLat = decimalLatitude,
         decimalLon = decimalLongitude,
         count,
         methodObs,
         source,
         dataOrigin) %>% 
  
  # Convert to sf and transform to Alaska Albers
  st_as_sf(coords = c("decimalLon", "decimalLat"),
           crs = 4326,
           remove = FALSE) %>% 
  st_transform(3338) %>% 
  
  # Eliminate repeat occurrences
  distinct() %>% 
  
  # Rearrange - this will order the data by the earliest date and then follow chronologically
  arrange(year, month, day)

# Save as csv into respective folder
write_csv(gbif_new, "../Data_Processed/TidyScripts/03-1_gbif_ambon_norseman_2015.csv", append = FALSE)

# Save rds and shps
# saveRDS(gbif_new, "../Data_Processed/rds_and_shps_new_tidy/03-1_gbif_ambon_norseman_2015.rds")
# st_write(gbif_new, "../Data_Processed/rds_and_shps_new_tidy/03-1_gbif_ambon_norseman_2015.shp")


