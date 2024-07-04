# Title: UAM Mammal Collections (Arctos)
# Author: Sydney Waloven
# Date: 2024-04-05
# Description: Data obtained from: https://www.gbif.org/dataset/377be098-626f-4cc2-b4b5-35700050669a

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
arctos <- read_csv(paste0(datadir, "16_uam_mammals_arctos/10_uam_mammals_arctos.csv"))


# data tidying ------------------------------------------------------------

# Check out column names
names(arctos)

# Look at what species are in this data set
arctos$species %>% unique()
# Contains all 3 species of interest

marine_mams <- arctos %>% 
  
  # Drop NAs in coordinates
  drop_na(decimalLatitude, decimalLongitude) %>% 
  
  # Remove unneeded columns and rename
  select(scientificName = verbatimScientificName,
         date = eventDate,
         decimalLat = decimalLatitude,
         decimalLon = decimalLongitude,
         count = individualCount,
         methodObs = basisOfRecord) %>% 
  
  # Subset to species of interst
  filter(scientificName %in% c("Odobenus rosmarus", "Erignathus barbatus", "Phoca largha")) %>% 
  
  # Standardize what's in the methodObs column and add source column
  mutate(methodObs = "Preserved specimen",
         source = "UAM Mammal Collections (Arctos)",
         count = 1,
         
         # Convert to POSIX format and create year, month, day columns
         date_posix = as.POSIXct(date, format = "%m/%d/%Y"),
         year = year(date_posix),
         month = month(date_posix),
         day = day(date_posix)) %>% # use / instead of - respectively
  
  # Remove old date column
  select(scientificName,
         year,
         month, 
         day,
         decimalLat,
         decimalLon,
         count,
         methodObs,
         source) %>% 
  
  # Remove occurrences outside of the study bounds
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


# Save as csv
write_csv(marine_mams, "../Data_Processed/TidyScripts/16_uam_collections_arctos.csv")

# Save as rds
# saveRDS(marine_mams, "../Data_Processed/16_uam_collections_arctos.rds")

# Save as shp
# st_write(marine_mams, "../Data_Processed/16_uam_collections_arctos.shp", append = FALSE)

