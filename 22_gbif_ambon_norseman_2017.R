# Title: Marine Mammal Data Paper Tidying 
# Author: Sydney Waloven
# Date: 2024-11-20
# Description: Vessel line-transect surveys of Arctic pinnipeds in the Chukchi Sea, Arctic Marine Biodiversity Observing Network (AMBON) research cruise on the vessel Norseman II, 2017-08-05 to 2017-08-25.

# https://www.gbif.org/dataset/bcaf91ed-a49f-4e34-8a20-fe8457986921

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

# This script will tidy pinniped data from the AMBON Norseman II cruise in 2017.
pinniped <- st_read(paste0(datadir, "22_gbif_ambon_norseman_2017/0023612-241107131044228.csv"))


# data tidying ------------------------------------------------------------

# Check out column names
names(pinniped)

# Replace empty strings and missing cell values with true NAs
na_pinniped <- pinniped %>% 
  mutate(
    across(
      everything(),
      ~na_if(., "")),
    across(
      everything(),
      ~na_if(., "missing"))) %>% 
  mutate(year = ifelse(year == 0, NA, year)) 

pinniped_new <- na_pinniped %>%
  
  # Drop NAs in coordinate, scientific name, and date columns
  drop_na(decimalLatitude, decimalLongitude, species, eventDate) %>% 
  
  # Add method of observation and source columns
  mutate(methodObs = "Line transect visual survey",
         source = "AMBON Norseman II Cruise 2017",
         
         # Convert columns to numeric
         decimalLat = as.numeric(decimalLatitude),
         decimalLon = as.numeric(decimalLongitude),
         year = as.numeric(year),
         month = as.numeric(month),
         day = as.numeric(day),
         
         # Add column to locate where the data was downloaded from
         dataOrigin = "GBIF") %>%
  
  # Filter to select our species of interest and occurrences within our study bounds
  filter(species %in% c("Erignathus barbatus", "Phoca largha", "Odobenus rosmarus"),
         decimalLat >= 46.08836 & decimalLat <= 73.92759, 
         decimalLon >= 172.18938 | decimalLon <= -146.30217) %>%
  
  # Remove unneeded columns and rename
  select(gbifID,
         scientificName = species,
         year,
         month,
         day,
         decimalLat,
         decimalLon,
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
write_csv(pinniped_new, "../Data_Processed/TidyScripts/22_gbif_ambon_norseman_2017.csv", append = FALSE)

# Save rds and shps
# saveRDS(mams_new, "../Data_Processed/rds_and_shps_new_tidy/22_gbif_ambon_norseman_2017.rds")
# st_write(mams_new, "../Data_Processed/rds_and_shps_new_tidy/22_gbif_ambon_norseman_2017.shp")

