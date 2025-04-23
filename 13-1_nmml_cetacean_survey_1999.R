# Title: NMML Cetaceans Survey 1999
# Author: Sydney Waloven
# Date: 2024-04-05
# Description: NMML Small Cetacean Aerial Survey 1999. Aerial survey from 5 June to 4 July, 1999 in Bristol Bay

# https://www.gbif.org/dataset/5c488b46-101b-470d-8b2b-ccf279488c04

# This script follows a standardized method for data tidying discussed in our data paper. 

# setup -------------------------------------------------------------------

# Load libraries
library(sf)
library(tidyverse)

# Clear environment history 
rm(list = ls())

# Set datadir
datadir <- "C:/Users/Natalia Portales/Michigan State University/Waloven, Sydney - ATP-MarineMammal-DataRaw/"
datadir <- "../../ATP-MarineMammal-DataRaw/"

# Read in csv
nmml <- st_read(paste0(datadir, "13_nmml_cetaceans_survey_1999/0024063-241107131044228.csv"))


# data tidying ------------------------------------------------------------

# Check out column names
names(nmml)

# Look at what species are in this data set
nmml$species %>% unique()
# Only species of interest that it contains for us is the walrus

# Replace empty strings and missing cell values with true NAs
na_nmml <- nmml %>% 
  mutate(
    across(
      everything(),
      ~na_if(., "")),
    across(
      everything(),
      ~na_if(., "missing"))) %>% 
  mutate(year = ifelse(year == 0, NA, year))

walrus <- na_nmml %>% 
  
  # Drop NAs in coordinates, species, and date columns
  drop_na(decimalLatitude, decimalLongitude, species, eventDate) %>% 
  
  # Add method of observation and source columns
  mutate(methodObs = "Aerial survey",
         source = "NMML Small Cetacean Aerial Survey 1999",
         
         # Convert appropriate columns to numeric
         decimalLatitude = as.numeric(decimalLatitude),
         decimalLongitude = as.numeric(decimalLongitude),
         year = as.numeric(year),
         month = as.numeric(month),
         day = as.numeric(day),
         
         # Add count column
         count = NA,
         
         # Add column to locate where the data was downloaded from
         dataOrigin = "GBIF") %>% 
  
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
  
  # Subset to species of interest and remove occurrences outside of study bounds
  filter(scientificName == "Odobenus rosmarus",
         decimalLat >= 46.08836 & decimalLat <= 73.92759, 
         decimalLon >= 172.18938 | decimalLon <= -146.30217) %>% 
  
  # Convert to sf and transform to Alaska Albers
  st_as_sf(coords = c("decimalLon", "decimalLat"),
           crs = 4326,
           remove = FALSE) %>% 
  st_transform(3338) %>% 
  
  # Eliminate repeat occurrences
  distinct() %>% 
  
  # Rearrange data by earliest date
  arrange(year, month, day)


# Save as csv
write_csv(walrus, "../Data_Processed/TidyScripts/13-1_gbif_nmml_cetacean_survey_1999.csv", append = FALSE)

# Save as rds
# saveRDS(walrus, "../Data_Processed/rds_and_shps_new_tidy/13-1_gbif_nmml_cetacean_survey_1999.rds")

# Save as shp
# st_write(walrus, "../Data_Processed/rds_and_shps_new_tidy/13-1_gbif_nmml_cetacean_survey_1999.shp")

