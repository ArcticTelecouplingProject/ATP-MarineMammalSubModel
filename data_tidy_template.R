# Title: Marine Mammal Data Paper Tidying Template
# Author: Sydney Waloven
# Date: 2024-01-08
# Description: This script is meant to serve as a template for tidying the marine mammal data sets we have collected to standardize the methods and create a data paper.


# setup -------------------------------------------------------------------

# Load libraries
library(sf)
library(tidyverse)

# Clear environment history 
rm(list=ls())

# Import data
# data_set <- read_csv("pathway/to/data/set.csv")

# This script will use the nmml aerial surveys from 1979-2010 as an example.
nmml <- read_csv("data/raw/8_nmml_aerial_1979_2010.csv")


# data tidying ------------------------------------------------------------

marine_mams <- nmml %>%
  
  # Drop NAs in coordinates
  drop_na(latitude, longitude) %>% 
  
  # Remove unneeded columns
  # Order of columns here determines order of columns in df. Can also rename columns in this function (new name on left, old name on right).
  select(id,
         scientific,
         common_name = common,
         date_time,
         latitude,
         longitude,
         count,
         method_obs = symbol,
         source = provider) %>% 
  
  # Convert to sf and transform to Alaska Albers
  st_as_sf(coords = c("longitude", "latitude"),
           crs = 4326,
           remove = FALSE) %>% 
  st_transform(3338) %>% 
  
  # Eliminate repeat occurrences
  distinct() %>% 
  
  # Rearrange
  arrange(date_time)

# Save as RDS
write_rds(marine_mams, "data/processed/nmml_aerial_surveys.rds")

# Read in tidied data
marine_mams <- read_rds("data/processed/nmml_aerial_surveys.rds")








