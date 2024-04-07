# Title: NMML Cetaceans Survey 1999
# Author: Sydney Waloven
# Date: 2024-04-05
# Description: Data obtained from: https://obis.org/dataset/0124b64b-16f3-40ec-b1a1-d8e9b2b7171f
# Aerial survey from 5 June to 4 July, 1999 in Bristol Bay

# Data number 7. Tidy script number 14.


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
nmml <- read_csv(paste0(datadir, "14_nmml_cetaceans_survey_1999/obis_seamap_dataset_117_points.csv"))


# data tidying ------------------------------------------------------------

# Check out column names
names(nmml)

# Look at what species are in this data set
nmml$scientific_name %>% unique()
# Only species of interest that it contains for us is the walrus

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
  
  # Subset to species of interest
  filter(scientificName == "Odobenus rosmarus") %>% 
  
  # Add method of observation and source columns
  mutate(methodObs = "Aerial survey",
         year = year(date_time),
         month = month(date_time),
         day = day(date_time),
         source = "NMML Cetaceans Survey 1999") %>% 
  
  # Remove occurrences outside of the study bounds
  filter(decimalLat >= 46.08836 & decimalLat <= 73.92759, 
         decimalLon >= 172.18938 | decimalLon <= -146.30217) %>% 
  
  # Convert to sf and transform to Alaska Albers
  st_as_sf(coords = c("decimalLon", "decimalLat"),
           crs = 4326,
           remove = FALSE) %>% 
  st_transform(3338) %>% 
  
  # Remove date_time col
  select(scientificName,
         year,
         month,
         day,
         decimalLat,
         decimalLon,
         count,
         methodObs,
         source) %>% 
  
  # Eliminate repeat occurrences
  distinct() %>% 
  
  # Rearrange data by earliest date
  arrange(year, month, day)


# Save as csv
write_csv(walrus, "../Data_Processed/14_nmml_cetacean_survey_1999.csv")

# Save as rds
saveRDS(walrus, "../Data_Processed/14_nmml_cetacean_survey_1999.rds")

