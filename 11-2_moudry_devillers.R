# Title: Marine Mammal Data Paper Tidying 
# Author: Natalia Portales and Sydney Waloven
# Date: 2024-01-27
# Description:  Source: GBIF. Quality and usability challenges of global marine biodiversity databases: An example for marine mammal data. https://www.gbif.org/occurrence/download/0024533-190918142434337

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

# This script will tidy GBIF data.
gbif_mams <- read_csv(paste0(datadir,"11_moudry_devillers/2-0003097-150811131857512.csv"))

# data tidying ------------------------------------------------------------

# Check out column names
names(gbif_mams)

gbif_mams_2 <- gbif_mams %>%
  
  # Drop NAs in coordinate, species, and year columns
  drop_na(decimallatitude, decimallongitude, species, year) %>% 
  
  # Filter to select species of interest by scientific name and spatial location within the study area bounds.
  filter(species %in% c("Odobenus rosmarus divergens", "Odobenus rosmarus"),
         decimallatitude >= 46.08836 & decimallatitude <= 73.92759, 
         decimallongitude >= 172.18938 | decimallongitude <= -146.30217) %>%
  
  # Modify and create new columns using mutate() function
  # Add source column
  mutate(source = "Moudry & Devillers Marine Mammal Walrus Data",
         
         # Rename values in method of observation column
         methodObs =
           case_when(basisofrecord == "HUMAN_OBSERVATION" ~ "Other",
                     basisofrecord == "PRESERVED_SPECIMEN" ~ "Preserved specimen", 
                     basisofrecord == "MACHINE_OBSERVATION" ~ "Other",
                     basisofrecord == "OBSERVATION" ~ "Other",
                     basisofrecord == "UNKNOWN" ~ "NA",
                     species == "Odobenus rosmarus divergens" ~ "Odobenus rosmarus",
                     TRUE ~ NA),
         
        # rename values in scientific name column
        scientificName =
            case_when(species == "Odobenus rosmarus divergens" ~ "Odobenus rosmarus",
                      species == "Odobenus rosmarus" ~ "Odobenus rosmarus"),
        
        # Add column to locate where the data was downloaded from
        dataOrigin = "GBIF") %>%
  
  # Add count column
  transform(count = ifelse
            (is.na(year) & is.na(month) & is.na(day), 0, NA)) %>%
  
  # Remove unneeded columns and rename
  select(gbifID = gbifid,
         scientificName,
         year,
         month,
         day,
         decimalLat = decimallatitude,
         decimalLon = decimallongitude,
         count,
         methodObs,
         source,
         dataOrigin) %>% 
  
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
write_csv(gbif_mams_2, "../Data_Processed/TidyScripts/11-2_gbif_moudry_devillers.csv", append = FALSE)

# Save rds and shps
# saveRDS(gbif_mams_2, "../Data_Processed/rds_and_shps_new_tidy/11-2_gbif_moudry_devillers.rds")
# st_write(gbif_mams_2, "../Data_Processed/rds_and_shps_new_tidy/11-2_gbif_moudry_devillers.shp", append = FALSE)