# Title: NMML Bering Sea Cetacean Survey 2000
# Author: Sydney Waloven
# Date: 2024-11-19
# Description: Data obtained from https://www.gbif.org/dataset/eae32224-05a2-4e3c-90f8-77b1d22a850c

# Visual line-transect surveys for cetaceans were conducted in the southeastern Bering Sea (SEBS) from 10 June to 3 July 2000, in association with a pollock stock assessment survey aboard the NOAA ship Miller Freeman.

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
nmml <- st_read(paste0(datadir, "16_nmml_bering_cetaceans_survey_2000/0022296-241107131044228.csv"))


# data tidying ------------------------------------------------------------

# Check out column names
names(nmml)

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

# Look at what species are in this data set
na_nmml$species %>% unique()
# Contains only walrus as our species of interest

walrus <- na_nmml %>% 
  
  # Drop NAs in coordinates
  drop_na(decimalLatitude, decimalLongitude, species, year) %>% 
  
  # Standardize what's in the methodObs column and add source column
  mutate(methodObs = "Line transect visual survey",
         source = "NMML Bering Sea Cetacean Survey 2000",
         
         # Convert appropriate columns to numeric
         decimalLatitude = as.numeric(decimalLatitude),
         decimalLongitude = as.numeric(decimalLongitude),
         year = as.numeric(year),
         month = as.numeric(month),
         day = as.numeric(day),
         individualCount = as.numeric(individualCount),
         
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
         count = individualCount,
         source,
         dataOrigin) %>% 
  
  # Subset to species of interest and remove occurrences outside of the study bounds
  filter(scientificName %in% c("Odobenus rosmarus", "Erignathus barbatus", "Phoca largha"),
         decimalLat >= 46.08836 & decimalLat <= 73.92759, 
         decimalLon >= 172.18938 | decimalLon <= -146.30217) %>% 
  
  # Convert to sf and transform to Alaska Albers
  st_as_sf(coords = c("decimalLon", "decimalLat"),
           crs = 4326,
           remove = FALSE) %>% 
  st_transform(3338) %>% 
  
  # Eliminate repeat occurrences
  distinct() %>% 
  
  # Rearrange - this will order the data by the earliest date and then follow chronologically
  arrange(year, month, day)


# Save as csv
write_csv(walrus, "../Data_Processed/TidyScripts/16-1_gbif_nmml_bering_survey_2000.csv", append = FALSE)

# Save as rds
# saveRDS(walrus, "../Data_Processed/rds_and_shps_new_tidy/16-1_gbif_nmml_bering_survey_2000.rds")

# Save as shp
# st_write(walrus, "../Data_Processed/rds_and_shps_new_tidy/16-1_gbif_nmml_bering_survey_2000.shp")
