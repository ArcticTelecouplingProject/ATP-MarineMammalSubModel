# Title: Marine Mammal Data Paper Tidying 
# Author: Natalia Portales and Sydney Waloven
# Date: 2024-01-27
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

# This script will use the nmml aerial surveys from 1979-2010 as example 1.
gbif_mams <- read_csv(paste0(datadir,"12_gbif_moudry/2-0003097-150811131857512.csv"))

# data tidying ------------------------------------------------------------

# Check out column names
names(gbif_mams)

gbif_mams_2 <- gbif_mams %>%
  
  # Drop NAs in coordinates, scientific name, and year
  drop_na(decimallatitude, decimallongitude, species, year) %>% 
  
  # Filter to select species of interest by scientific name and spacial location within the study area bounds.
  filter(species %in% c("Odobenus rosmarus divergens", "Odobenus rosmarus"),
         decimallatitude >= 62 & decimallatitude <= 66.5, 
         decimallongitude >= -175 & decimallongitude <= -164) %>%
  
  # Add method of observation and source columns
  mutate(source = "Moudry GBIF Marine Mammal Walrus Data",
         
         # Rename values of method column
         methodObs =
           case_when(basisofrecord == "HUMAN_OBSERVATION" ~ "Human observation",
                     basisofrecord == "PRESERVED_SPECIMEN" ~ "Preserved specimen", 
                     basisofrecord == "MACHINE_OBSERVATION" ~ "Machine observation",
                     basisofrecord == "OBSERVATION" ~ "Observation",
                     basisofrecord == "UNKNOWN" ~ "NA",
                     species == "Odobenus rosmarus divergens" ~ "Odobenus rosmarus",
                     TRUE ~ NA), 
        # rename values in scientific name column
        scientificName =
            case_when(species == "Odobenus rosmarus divergens" ~ "Odobenus rosmarus",
                      species == "Odobenus rosmarus" ~ "Odobenus rosmarus")) %>%
  
  # Remove unneeded columns and rename
  # Order of columns here determines order of columns in df. Can also rename columns in this function (new name on left, old name on right).
  select(scientificName,
         year,
         month,
         day,
         decimalLat = decimallatitude,
         decimalLon = decimallongitude,
         methodObs) %>% 
  
  #Add count column
  transform(count = ifelse
              (is.na(year) & is.na(month) & is.na(day), 0, 1)) %>%
  
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
write_csv(gbif_mams_2, "../Data_Processed/12-2_gbif_moudry/12-2-1_gbif_moudry_tidy.csv")

# Read in tidied data
# gbif_mams_1 <- read_csv("../Data_Processed/12-2_gbif_moudry/12-2-1_gbif_moudry_tidy.csv")
