# Title: Marine Mammal Data Paper Tidying 
# Author: Natalia Portales and Sydney Waloven
# Date: 2024-03-24
# Description: Source: TGS Seismic Survey Visual Sightings in Chukchi Sea 2013. https://obis.org/dataset/4b394534-8a6b-4bdb-bf1b-f07f983f9e37. This script follows standardized methods of tidying for the marine mammal data sets we have collected contributing to a data paper.

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

# This script will tidy obis survey data from 2013.
obis_mams <- read.csv(paste0(datadir, "2_obis_tgs_seismic_2013/obis_seamap_dataset_1110_csv_71491/obis_seamap_dataset_1110_points.csv"))

# data tidying ------------------------------------------------------------

# Check out column names
names(obis_mams)

obis_new <- obis_mams %>%
  
  # Drop NAs in coordinates, scientific name, and date columns
  drop_na(latitude, longitude, scientific_name, date_time) %>% 
  
  # Filter to select species of interest by scientific name and spatial location within the study area bounds.
  filter(scientific_name %in% c("Erignathus barbatus", "Phoca largha", "Odobenus rosmarus",
                               "Balaena mysticetus", "Cetacea", "Pusa hispida", 
                               "Eschrichtius robustus", "Phocoena phocoena", 
                               "Megaptera novaeangliae", "Mysticeti", 
                               "Balaenoptera acutorostrata", "Histriophoca fasciata"),
         latitude >= 46.08836 & latitude <= 73.92759, 
         longitude >= 172.18938 | longitude <= -146.30217) %>%
  
  # Add method of observation and source columns
  mutate(methodObs = "Seismic vessel visual observations",
         source = "TGS Seismic 2013",
         
         # format date column to a standard format
         date_POSIXct = as.POSIXct(date_time), 
         
         # Add year, month, day columns
         year = as.numeric(substr(date_POSIXct, 1, 4)),
         month = as.numeric(substr(date_POSIXct, 6, 7)),
         day = as.numeric(substr(date_POSIXct, 9, 10))) %>%
  
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

# Save as csv into respective folder - add tidy to the end of file names so we can differentiate easily
write_csv(obis_new, "../Data_Processed/TidyScripts/2-1_tidy_obis_tgs_seismic_2013.csv")

# Read in tidied data
# marine_mams <- read_csv("../Data_Processed/TidyScripts/2-1_tidy_obis_tgs_seismic_2013.csv")

