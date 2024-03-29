# Title: Marine Mammal Data Paper Tidying 
# Author: Natalia Portales and Sydney Waloven
# Date: 2024-01-26
# Description:  Source: GBIF. Quality and usability challenges of global marine biodiversity databases: An example for marine mammal data. https://www.gbif.org/occurrence/download/0024533-190918142434337

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

# This script will tidy GBIF data.
gbif_mams <- read_csv(paste0(datadir,"12_gbif_moudry/1-0024533-190918142434337.csv"))

# data tidying ------------------------------------------------------------
       
# Check out column names
names(gbif_mams)

gbif_mams_1 <- gbif_mams %>%
  
  # Drop NAs in coordinates, species, and year columns
  drop_na(decimalLatitude, decimalLongitude, verbatimScientificName, year) %>%
  
  # Filter to select species of interest by scientific name and spatial location within the study area bounds.
  filter(verbatimScientificName %in% c("Odobenus rosmarus divergens", "Odobenus rosmarus"),
         decimalLatitude >= 46.08836 & decimalLatitude <= 73.92759, 
         decimalLon >= 172.18938 | decimalLon <= -146.30217) %>%
  
  # Modify and create new columns using mutate() function 
  # Add source column
  mutate(source = "Moudry GBIF Marine Mammal Walrus Data",
         
         # Rename values in method of observation column
         methodObs =
           case_when(basisOfRecord == "HUMAN_OBSERVATION" ~ "Human observation",
                     basisOfRecord == "PRESERVED_SPECIMEN" ~ "Preserved specimen", 
                     basisOfRecord == "MACHINE_OBSERVATION" ~ "Machine observation",
                     basisOfRecord == "OBSERVATION" ~ "Observation",
                     basisOfRecord == "UNKNOWN" ~ "NA",
                     TRUE ~ NA),
         
        # rename values in scientific name column
        scientificName =
          case_when(verbatimScientificName == "Odobenus rosmarus divergens" ~ "Odobenus rosmarus",
                    verbatimScientificName == "Odobenus rosmarus" ~ "Odobenus rosmarus",
                    TRUE ~ NA)) %>%
            
  # Remove unneeded columns and rename
  select(scientificName,
         year,
         month,
         day,
         decimalLat = decimalLatitude,
         decimalLon = decimalLongitude,
         count = individualCount,
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

# Save as csv into respective folder - add tidy to the end of file names so we can differentiate easily
write_csv(gbif_mams_1, "../Data_Processed/12-1_gbif_moudry/12-1-1_gbif_moudry_tidy.csv")

# Read in tidied data
# marine_mams <- read_csv("../Data_Processed/12-1-1_gbif_moudry_tidy.csv")

