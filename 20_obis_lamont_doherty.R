# Title: Marine Mammal Data Paper Tidying 
# Author: Sydney Waloven
# Date: 2024-07-12
# Description: Source: Lamont-Doherty/LGL/NSF cruises https://obis.org/dataset/60ce413e-2d58-4d64-a057-7eed1b201250

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

# This script will tidy OBIS survey data from a marine mammal and sea turtle monitoring program.
obis_mams <- read.csv(paste0(datadir, "20_obis_lamont_doherty/obis_seamap_dataset_511_points.csv"))

# data tidying ------------------------------------------------------------

# Check out column names
names(obis_mams)

obis_new <- obis_mams %>%
  
  # Drop NAs in coordinates, scientific name, and date columns
  drop_na(latitude, longitude, scientific_name, date_time) %>% 
  
  # Filter to select species of interest by scientific name and spatial location within the study area bounds.
  filter(scientific_name %in% c("Odobenus rosmarus", "Phoca largha", "Erignathus barbatus"), 
         latitude >= 46.08836 & latitude <= 73.92759, 
         longitude >= 172.18938 | longitude <= -146.30217) %>%
  
  # Add method of observation and source columns
  mutate(methodObs = "Seismic survey visual observation",
         source = "Lamont-Doherty/LGL/NSF cruises",
         
         # Format date column to a standard format
         date = as.Date(date_time, "%Y-%m-%d"), 
         
         # Add year, month, day columns
         year = year(date),
         month = month(date),
         day = day(date)) %>%
  
  # Remove unneeded columns and rename
  select(scientificName = scientific_name,
         year,
         month,
         day,
         decimalLat = latitude,
         decimalLon = longitude,
         count = group_size,
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
write_csv(obis_new, "../Data_Processed/TidyScripts/20_obis_lamont_doherty.csv")

# Save as rds and shp files
# saveRDS(obis_new, "../Data_Processed/rds_and_shps_new_tidy/20_obis_lamont_doherty.rds")
# st_write(obis_new, "../Data_Processed/rds_and_shps_new_tidy/20_obis_lamont_doherty.shp",
#          append = FALSE)


