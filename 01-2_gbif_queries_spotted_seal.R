# Title: Marine Mammal Data Paper Tidying 
# Author: Natalia Portales and Sydney Waloven
# Date: 2024-04-12
# Description: This data set was obtained from a query on Global Biodiversity Information Facility (GBIF): https://www.gbif.org/

# Only species featured in this script is the spotted seal (Phoca largha).

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

# This script will tidy gbif spotted seal data.
spotted_gbif <- st_read(paste0(datadir, "01_gbif_queries/gbif_spotted_20240703.csv"))

# data tidying ------------------------------------------------------------

# Check out column names
names(spotted_gbif)

# Replace empty strings and missing cell values with true NAs
na_gbif <- spotted_gbif %>% 
  mutate(
    across(
      everything(),
      ~na_if(., "")),
    across(
      everything(),
      ~na_if(., "missing"))) %>% 
  mutate(year = ifelse(year == 0, NA, year)) 

gbif_new_spotted <- na_gbif %>%
  
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
      case_when(basisOfRecord == "HUMAN_OBSERVATION" ~ "Other",
                basisOfRecord == "MACHINE_OBSERVATION" ~ "Other",
                basisOfRecord == "PRESERVED_SPECIMEN" ~ "Preserved specimen",
                TRUE ~ NA_character_),
    
    # Add GBIF source column
    source_gbif = "GBIF Spotted Seal Query |",
    
    # Merge collector source and data source contents together
    source = paste(source_gbif, recordedBy, sep = " "),
    
    # Add column to locate where the data was downloaded from
    dataOrigin = "GBIF") %>% 
  
  # Remove unneeded columns and rename
  select(scientificName = species,
         year,
         month,
         day,
         decimalLat,
         decimalLon,
         count,
         methodObs,
         source,
         dataOrigin) %>% 
  
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

# Save as csv into respective folder
write_csv(gbif_new_spotted, "../Data_Processed/TidyScripts/01-2_gbif_queries_spotted_seal.csv", append = FALSE)

# Save as rds and shp
# saveRDS(gbif_new_spotted, "../Data_Processed/rds_and_shps_new_tidy/01-2_gbif_queries_spotted_seal.rds")
# st_write(gbif_new_spotted, "../Data_Processed/rds_and_shps_new_tidy/01-2_gbif_queries_spotted_seal.shp", append = FALSE)
