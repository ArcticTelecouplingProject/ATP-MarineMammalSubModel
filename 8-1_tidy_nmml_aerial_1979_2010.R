# Title: Marine Mammal Data Paper Tidying 
# Author: Natalia Portales and Sydney Waloven
# Date: 2024-01-14
# Description: Dataset comes from the Arctic Data Center and was obtained from Marine Mammal aerial surveys in the Bering, Chukchi, and Beaufort Seas 1979-2010. https://www.gbif.org/dataset/e80fa6fc-5468-4ee7-8a74-e6dea5ccfaf9#description

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

# This script will tidy nmml aerial surveys from 1979-2010.
nmml <- read_csv(paste0(datadir, "8_nmml_aerial_1979_2010/8_nmml_aerial_1979_2010.csv"))


# data tidying ------------------------------------------------------------

# Check out column names
names(nmml)

marine_mams <- nmml %>%
  
  # Drop NAs in coordinate, species, and year columns
  drop_na(decimalLatitude, decimalLongitude, species, year) %>% 
 
  # Filter to select species of interest by scientific name and spatial location within the study area bounds
  filter(species %in% c("Odobenus rosmarus", "Phoca largha", "Erignathus barbatus"), 
         decimalLatitude >= 46.08836 & decimalLatitude <= 73.92759, 
         decimalLongitude >= 172.18938 | decimalLongitude <= -146.30217) %>% 
  
  # Modify and create new columns using mutate() function  
  # Add method of observation and source columns
  mutate(methodObs = "Aerial survey",
         source = "NMML Marine Mammal Aerial Surveys 1979-2010") %>% 
  
  # Remove unneeded columns and rename
  select(scientificName = species,
         year,
         month,
         day,
         decimalLat = decimalLatitude,
         decimalLon = decimalLongitude,
         count = individualCount,
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
write_csv(marine_mams, "../Data_Processed/TidyScripts/8-1_tidy_nmml_aerial_1979_2010.csv")

# Save rds and shps
# saveRDS(marine_mams, "../Data_Processed/rds_and_shps_new_tidy/8-1_tidy_nmml_aerial_1979_2010.rds")
# st_write(marine_mams, "../Data_Processed/rds_and_shps_new_tidy/8-1_tidy_nmml_aerial_1979_2010.shp", append = FALSE)

# Read in tidied data
# marine_mams <- read_csv("../Data_Processed/TidyScripts/8-1_tidy_nmml_aerial_1979_2010.csv")

