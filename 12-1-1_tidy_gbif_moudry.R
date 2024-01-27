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

# This script will use the nmml aerial surveys from 1979-2010 as example 1.
gbif_mams <- read_csv(paste0(datadir,"12_gbif_moudry/1-0024533-190918142434337.csv"))

# data tidying ------------------------------------------------------------

# Check out column names
names(gbif_mams)

gbif_mams_1 <- gbif_mams %>%
  
  # Drop NAs in coordinates
  drop_na(decimalLatitude, decimalLongitude) %>% 
  
  # Add method of observation and source columns
  mutate(source = "Moudry GBIF Marine Mammal Walrus Data",

# Rename values of method column
        methodobs =
           case_when(basisOfRecord == HUMAN_OBSERVATION ~ "Human observation",
                     basisOfRecord == PRESERVED_SPECIMEN ~ "Preserved specimen", 
                     basisOfRecord == MACHINE_OBSERVATION ~ "Machine observation",
                     basisOfRecord == OBSERVATION ~ "Observation",
                     basisOfRecord == UNKNOWN ~ "NA",
                     TRUE ~ NA)) %>% 
  
  # Remove unneeded columns and rename
  # Order of columns here determines order of columns in df. Can also rename columns in this function (new name on left, old name on right).
  select(scientificName = verbatimScientificName,
         year,
         month,
         day,
         decimalLat = decimalLatitude,
         decimalLon = decimalLongitude,
         count = individualCount,
         methodObs = basisOfRecord) %>% 
  
  # Subset to species of interest or comment out to see data for all species
  # filter(scientificName == c("Erignathus barbatus", "Phoca largha", "Odobenus rosmarus")) %>%

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
# gbif_mams_1 <- read_csv("../Data_Processed/12-1_gbif_moudry/12-1-1_gbif_moudry_tidy")
