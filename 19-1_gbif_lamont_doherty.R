# Title: Marine Mammal Data Paper Tidying 
# Author: Sydney Waloven
# Date: 2024-11-19
# Description: Lamont-Doherty/LGL/NSF cruises https://www.gbif.org/occurrence/download/0022723-241107131044228

# This script follows a standardized method for data tidying discussed in our data paper.

# setup -------------------------------------------------------------------

# Load libraries
library(sf)
library(tidyverse)

# Clear environment history 
rm(list = ls())

# Import data
# data_set <- read_csv("pathway/to/data/set.csv")

# This example will use the datadir method that we used as a workaround for the file path issue of the raw data.
datadir <- "C:/Users/Natalia Portales/Michigan State University/Waloven, Sydney - ATP-MarineMammal-DataRaw/"
datadir <- "../../ATP-MarineMammal-DataRaw/"

# This script will tidy OBIS survey data from a marine mammal and sea turtle monitoring program.
gbif_mams <- st_read(paste0(datadir, "19_obis_lamont_doherty/0022723-241107131044228.csv"))

# data tidying ------------------------------------------------------------

# Check out column names
names(gbif_mams)

# Replace empty strings and missing cell values with true NAs
na_gbif <- gbif_mams %>% 
  mutate(
    across(
      everything(),
      ~na_if(., "")),
    across(
      everything(),
      ~na_if(., "missing"))) %>% 
  mutate(year = ifelse(year == 0, NA, year))

gbif_new <- na_gbif %>%
  
  # Drop NAs in coordinates, scientific name, and date columns
  drop_na(decimalLatitude, decimalLongitude, species, year) %>% 
  
  # Add method of observation and source columns
  mutate(methodObs = "Seismic survey visual observation",
         source = "Lamont-Doherty/LGL/NSF cruises",
         
         # Convert appropriate columns to numeric
         decimalLatitude = as.numeric(decimalLatitude),
         decimalLongitude = as.numeric(decimalLongitude),
         year = as.numeric(year),
         month = as.numeric(month),
         day = as.numeric(day),
         individualCount = as.numeric(individualCount),
         
         # Add column to locate where the data was downloaded from
         dataOrigin = "GBIF") %>%
  
  # Filter to select species of interest by scientific name and spatial location within the study area bounds.
  filter(species %in% c("Odobenus rosmarus", "Phoca largha", "Erignathus barbatus"), 
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
         count = individualCount,
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
write_csv(gbif_new, "../Data_Processed/TidyScripts/19-1_gbif_lamont_doherty.csv", append = FALSE)

# Save as rds and shp files
# saveRDS(obis_new, "../Data_Processed/rds_and_shps_new_tidy/19-1_obis_lamont_doherty.rds")
# st_write(obis_new, "../Data_Processed/rds_and_shps_new_tidy/19-1_obis_lamont_doherty.shp",
#          append = FALSE)


