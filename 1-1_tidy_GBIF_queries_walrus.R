# Title: Marine Mammal Data Paper Tidying 
# Author: Natalia Portales and Sydney Waloven
# Date: 2024-03-24
# Description: Source: NSF ARCSS Google Drive. Occurrence data of the three species to be evaluated in the project: Pacific walrus (Odobenus rosmarus divergens), bearded seal (Erignathus barbatus), and spotted seal (Phoca largha).

# These datasets were obtained from the Global Biodiversity Information Facility (GBIF): https://www.gbif.org/ 

# Only species featured in this script is Pacific walrus.

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

# This script will tidy gbif walrus data.
walrus_gbif <- st_read(paste0(datadir, "1_arcss_gbif/odobenus_rosmarus_divergens"))

# data tidying ------------------------------------------------------------

# Check out column names
names(walrus_gbif)

gbif_new <- walrus_gbif %>%
  
  # Drop NAs in coordinate, scientific name, and year columns
  drop_na(decimalLat, decimalLon, species, year) %>% 
  
  # Modify and create new columns using mutate() function 
  # Add source column
  mutate(source = "ARCSS GBIF Pacific Walrus",
         
         # Convert columns to numeric
         decimalLat = as.numeric(decimalLat),
         decimalLon = as.numeric(decimalLon),
         year = as.numeric(year),
         month = as.numeric(month),
         day = as.numeric(day),
         
         # Add count column
         count = 1,
         
         # Rename values in method of observation column
         methodObs =
           case_when(basisOfRec == '"HUMAN_OBSERVATION"' ~ "Human observation",
                     basisOfRec == '"PRESERVED_SPECIMEN"' ~ "Preserved specimen",
                     TRUE ~ NA),
         
         # rename values in scientific name column
         scientificName =
           case_when(species == '"Odobenus rosmarus"' ~ "Odobenus rosmarus",
                     TRUE ~ NA)) %>%
  
  # Remove unneeded columns and rename
  select(scientificName,
         year,
         month,
         day,
         decimalLat,
         decimalLon,
         count,
         methodObs,
         source) %>% 
  
  # Filter to select occurrences with spatial location within the study area bounds
  filter(decimalLat >= 46.08836 & decimalLat <= 73.92759, 
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

# Save as csv into respective folder - add tidy to the end of file names so we can differentiate easily
write_csv(gbif_new, "../Data_Processed/1-1_ARCSS_GBIF_walrus/1-1_ARCSS_GBIF_walrus_tidy.csv")

# Read in tidied data
# marine_mams <- read_csv("../Data_Processed/6-1_nsf_dbo_laurier_2019_tidy.csv")

