# Title: NMML Bering Sea Cetacean Survey 2000
# Author: Sydney Waloven
# Date: 2024-04-05
# Description: Data obtained from: https://obis.org/dataset/4176d63d-2ac2-42c4-9010-901856b6db0e

# Visual line-transect surveys for cetaceans were conducted in the southeastern Bering Sea (SEBS) from 10 June to 3 July 2000, in association with a pollock stock assessment survey aboard the NOAA ship Miller Freeman.

# Data number 19. Tidy script number 17


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
nmml <- read_csv(paste0(datadir, "17_nmml_bering_cetaceans_survey_2000/obis_seamap_dataset_122_points.csv"))


# data tidying ------------------------------------------------------------

# Check out column names
names(nmml)

# Look at what species are in this data set
nmml$scientific_name %>% unique()
# Contains only walrus

walrus <- nmml %>% 
  
  # Drop NAs in coordinates
  drop_na(latitude, longitude) %>% 
  
  # Remove unneeded columns and rename
  select(scientificName = scientific_name,
         date_time,
         decimalLat = latitude,
         decimalLon = longitude,
         count = group_size,
         source = provider) %>% 
  
  # Subset to species of interst
  filter(scientificName %in% c("Odobenus rosmarus", "Erignathus barbatus", "Phoca largha")) %>% 
  
  # Standardize what's in the methodObs column and add source column
  mutate(methodObs = "Visual line transect survey",
         source = "NMML Bering Sea Cetacean Survey 2000",
         year = year(date_time),
         month = month(date_time),
         day = day(date_time)) %>%
  
  # Remove old date column and reorder
  select(scientificName,
         year,
         month, 
         day,
         decimalLat,
         decimalLon,
         count,
         methodObs) %>% 
  
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
write_csv(walrus, "../Data_Processed/17_nmml_bering_survey_2000.csv")

# Save as rds
saveRDS(walrus, "../Data_Processed/17_nmml_bering_survey_2000.rds")
