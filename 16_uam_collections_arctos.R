# Title: UAM Mammal Collections (Arctos)
# Author: Sydney Waloven
# Date: 2024-04-05
# Description: Data obtained from: https://www.gbif.org/dataset/377be098-626f-4cc2-b4b5-35700050669a

# Data number 10. Tidy script number 16



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
         source = "UAM Mammal Collections (ARCTOS)")

# Create function to extract numbers before a specific character
extract_num <- function(string, separator) {
  parts <- unlist(strsplit(string, separator))
  as.numeric(parts[1])
}

# Apply function across date column
sapply(marine_mams$date, extract_num, separator = "/")
        
  
  # Remove old date column
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
  arrange(year)


