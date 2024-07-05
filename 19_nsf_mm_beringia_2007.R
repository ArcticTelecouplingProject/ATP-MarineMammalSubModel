# Title: Observations of Marine Mammals of Beringia 2007
# Author: Sydney Waloven
# Date: 2024-04-06
# Description: Data obtained from: https://arcticdata.io/catalog/view/doi%3A10.5065%2FD67S7KS1

# This dataset contains marine mammal observations onboard the US Coast Guard Cutter Healy during the 2007 0701 and 0702 cruises in the Bering Sea. The objective of this project has been to observe marine mammals in the context of their sea-ice "seascape" habitat. Emphasis has been placed on the winter-spring period during which Pacific walruses and ribbon seals use sea ice for reproduction, molting, rest, and as a platform for feeding and migration. The thesis is that marine mammals partition sea-ice habitat according to their life-history requirements: for Pacific walruses, "broken pack" and for ribbon seals "loose pack" provide this habitat.

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
nsf_2007 <- read_csv(paste0(datadir, "19_nsf_mm_beringia_2007/data/HLY0701-02-Wal-comb-obs-4.14-6.10.csv"))


# data tidying ------------------------------------------------------------

# Check out column names
names(nsf_2007)

# Remove quotations from column names
names(nsf_2007) <- gsub("'", "", names(nsf_2007))

walrus <- nsf_2007 %>% 
  
  # Drop NAs in coordinates
  drop_na(Latitude, Longitude) %>% 
  
  # Remove unneeded columns and rename
  select(year = Year,
         month = Month,
         day = Day,
         decimalLat = Latitude,
         decimalLon = Longitude,
         count = Number) %>% 
  
  # Standardize what's in the methodObs column and add source and scientificName column
  mutate(scientificName = "Odobenus rosmarus",
         methodObs = "Research cruise visual observation survey",
         source = "Marine Mammal Beringia Survey (Healy) 2007") %>%
  
  # Remove old date column and reorder
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

# If commenting out the spatial bounds filter, then this will fix occurrences where the year is not 4 digits. But if keeping in the spatial bounds filter, then these occurrences are removed.
# walrus$year[1] <- 2007

# walrus$year[196] <- 2007


# Save as csv
write_csv(walrus, "../Data_Processed/TidyScripts/19_nsf_mm_beringia_2007.csv")

# Save as rds
# saveRDS(walrus, "../Data_Processed/rds_and_shps_new_tidy/19_nsf_mm_beringia_2007.rds")

# Save as shp
# st_write(walrus, "../Data_Processed/rds_and_shps_new_tidy/19_nsf_mm_beringia_2007.shp")
