# Title: Marine Mammal Data Paper Tidying 
# Author: Natalia Portales and Sydney Waloven
# Date: 2024-04-12
# Description: Source: GBIF. This data set was obtained from a query on Global Biodiversity Information Facilit (GBIF): https://www.gbif.org/

# Only species featured in this script is Pacific walrus (Odobenus rosmarus).

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
walrus_gbif <- st_read(paste0(datadir, "1-1_gbif_queries/gbif_odobenus_20240329.csv"))


# data tidying ------------------------------------------------------------

# Check out column names
names(walrus_gbif)

# Replace empty strings and missing cell values with true NAs
na_gbif <- walrus_gbif %>% 
  mutate(
    across(
      everything(),
      ~na_if(., "")),
    across(
      everything(),
      ~na_if(., "missing"))) %>% 
  mutate(year = ifelse(year == 0, NA, year)) 

gbif_new <- na_gbif %>%
  
  # Drop NAs in coordinate, scientific name, and year columns
  drop_na(decimalLatitude, decimalLongitude, species, year) %>% 
  
  # Modify and create new columns using mutate() function 
  mutate(
    # Convert columns to numeric
    decimalLat = as.numeric(decimalLatitude),
    decimalLon = as.numeric(decimalLongitude),
    year = as.numeric(year),
    month = as.numeric(month),
    day = as.numeric(day),
    
    # Add count column
    count = 1,
    
    # Rename values in method of observation column
    methodObs =
      case_when(basisOfRecord == "HUMAN_OBSERVATION" ~ "Human observation",
                basisOfRecord == "MACHINE_OBSERVATION" ~ "Machine observation",
                basisOfRecord == "PRESERVED_SPECIMEN" ~ "Preserved specimen",
                TRUE ~ NA_character_),
    
    # Add GBIF source column
    source_gbif = "GBIF Query |") %>% 
  
  # Merge collector source and data source contents together
  mutate(source = 
           paste(source_gbif, recordedBy, sep = " ")) %>% 
  
  # Remove unneeded columns and rename
  select(scientificName = species,
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
  
  # Eliminate duplicate records
  distinct() %>% 
  
  # Rearrange - this will order the data by the earliest date and then follow chronologically
  arrange(year, month, day)


# Save as csv into respective folder - add tidy to the end of file names so we can differentiate easily
write_csv(gbif_new, "../Data_Processed/TidyScripts/1-1_tidy_GBIF_queries_walrus.csv", append = FALSE)

# Save as rds and shp
saveRDS(gbif_new, "../Data_Processed/rds_and_shps_new_tidy/1-1_tidy_GBIF_queries_walrus.rds")
st_write(gbif_new, "../Data_Processed/rds_and_shps_new_tidy/1-1_tidy_GBIF_queries_walrus.shp")

# Read in tidied data
# marine_mams <- read_csv("../Data_Processed/1-1_tidy_GBIF_queries_walrus.csv")

