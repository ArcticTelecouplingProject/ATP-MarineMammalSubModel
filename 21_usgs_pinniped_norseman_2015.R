# Title: Marine Mammal Data Paper Tidying 
# Author: Sydney Waloven
# Date: 2024-09-23
# Description: Vessel line-transect surveys of Arctic pinnipeds in the Chukchi Sea, Arctic Marine Biodiversity Observing Network (AMBON) research cruise on the vessel Norseman II, 2015-08-09 to 2015-09-03
# https://www.sciencebase.gov/catalog/item/5e433929e4b0edb47be846f3

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

# This script will tidy marine mammal data from the AMBON Norseman II cruise in 2015.

# Occurrence data
pinniped_occ <- read_csv(paste0(datadir, "21_gbif_ambon_norseman_2015/pinnipeds_2015_dwc_occurrence.csv"))

# Locations data - this data set has lat and long connected to birdocc via eventID column
pinniped_event <- read_csv(paste0(datadir,"21_gbif_ambon_norseman_2015/pinnipeds_2015_dwc_event.csv"))

# Merge via the shared "eventID" column
pinniped <- left_join(pinniped_occ, pinniped_event, by = c("eventID"))
head(pinniped) # Now we've picked the matching lines from the pinniped event and moved them over to the pinniped occ


# data tidying ------------------------------------------------------------

# Check out column names
names(pinniped)

mams_new <- pinniped %>%
  
  # Drop NAs in coordinate, scientific name, and date columns
  drop_na(decimalLatitude, decimalLongitude, scientificName, eventDate) %>% 
  
  # Filter to "Best Estimate" for count of animals observed
  filter(organismQuantityType == "Best Estimate",
         # Filter to select our species of interest and occurrences within our study bounds
         scientificName %in% c("Erignathus barbatus", "Phoca largha", "Odobenus rosmarus"),
         decimalLatitude >= 46.08836 & decimalLatitude <= 73.92759, 
         decimalLongitude >= 172.18938 | decimalLongitude <= -146.30217) %>%
  
  # Add method of observation and source columns
  mutate(methodObs = "Line transect visual survey",
         source = "AMBON Norseman II Cruise 2015",
         year_date = year(eventDate),
         month_date = month(eventDate),
         day_date = day(eventDate),
         
         # Add column to locate where the data was downloaded from
         dataOrigin = "USGS") %>%
  
  # Remove unneeded columns and rename
  select(scientificName,
         year = year_date,
         month = month_date,
         day = day_date,
         decimalLat = decimalLatitude,
         decimalLon = decimalLongitude,
         count = organismQuantity,
         methodObs,
         source,
         dataOrigin) %>% 
  
  # Convert to sf and transform to Alaska Albers
  st_as_sf(coords = c("decimalLon", "decimalLat"),
           crs = 4326,
           remove = FALSE) %>% 
  st_transform(3338) %>% 
  
  # Eliminate repeat occurrences
  distinct(year, month, day, decimalLat, decimalLon,
           .keep_all = TRUE) %>%
  
  # Rearrange - this will order the data by the earliest date and then follow chronologically
  arrange(year, month, day)

# Save as csv into respective folder
write_csv(mams_new, "../Data_Processed/TidyScripts/21_usgs_ambon_norseman_2015.csv", append = FALSE)

# Save rds and shps
# saveRDS(mams_new, "../Data_Processed/rds_and_shps_new_tidy/21_usgs_ambon_norseman_2015.rds")
# st_write(mams_new, "../Data_Processed/rds_and_shps_new_tidy/21_usgs_ambon_norseman_2015.shp")

