# Title: Marine Mammal Data Paper Tidying 
# Author: Natalia Portales and Sydney Waloven
# Date: 2024-03-23
# Description: Source: OBIS. This dataset was obtained from: AMBON Chukchi Sea research cruse on the Norseman II. https://www.sciencebase.gov/catalog/item/5e0e24b3e4b0b207aa13782b

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

# This script will tidy obis survey data from 2015. Two data sets need to be combined to include all data variables.

# Occurrence data
birdocc <- read.csv(paste0(datadir, "3_obis_ambon_norseman_2015/dlog_2015_dwc_occurrence.csv"))

# Locations data - this data set has lat and long connected to birdocc via eventID column
birdevent <- read.csv(paste0(datadir,"3_obis_ambon_norseman_2015/dlog_2015_dwc_event.csv"))

# Merge via the shared "eventID" column
bird <- left_join(birdocc, birdevent, by=c("eventID"))
head(bird) # Now we've picked the matching lines from the birdevent and moved them over to the bird occ

# data tidying ------------------------------------------------------------

# Check out column names
names(bird)

bird_new <- bird %>%
  
  # Drop NAs in coordinate, scientific name, and date columns
  drop_na(decimalLatitude, decimalLongitude, scientificName, eventDate) %>% 
  
  # Filter to select our species of interest and occurrences within our study bounds
  filter(scientificName %in% c("Erignathus barbatus", "Phoca largha", "Odobenus rosmarus"),
         decimalLatitude >= 46.08836 & decimalLatitude <= 73.92759, 
         decimalLongitude >= 172.18938 | decimalLongitude <= -146.30217) %>%
  
  # Modify and create new columns using mutate() function
  # Add method of observation and source columns
  mutate(methodObs = "Line transect visual survey",
         source = "AMBON Norseman II Cruise 2015",
         
         # format date column to a standard format
         date_POSIXct = as.POSIXct(eventDate), 
         
         # Add year, month, day columns
         year = as.numeric(substr(date_POSIXct, 1, 4)),
         month = as.numeric(substr(date_POSIXct, 6, 7)),
         day = as.numeric(substr(date_POSIXct, 9, 10))) %>%
  
  # Remove unneeded columns and rename
  select(scientificName,
         year,
         month,
         day,
         decimalLat = decimalLatitude,
         decimalLon = decimalLongitude,
         count = individualCount,
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

# Save as csv into respective folder
write_csv(bird_new, "../Data_Processed/TidyScripts/3_obis_ambon_norseman_2015.csv")

# Save rds and shps
# saveRDS(bird_new, "../Data_Processed/rds_and_shps_new_tidy/3_1_tidy_obis_ambon_norseman_2015.rds")
# st_write(bird_new, "../Data_Processed/rds_and_shps_new_tidy/3_1_tidy_obis_ambon_norseman_2015.shp")

# Read in tidied data
# marine_mams <- read_csv("../Data_Processed/TidyScripts/3-1_tidy_obis_ambon_norseman_2015.csv")

