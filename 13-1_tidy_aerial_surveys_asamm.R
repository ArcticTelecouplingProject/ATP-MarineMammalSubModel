# Title: Marine Mammal Data Paper Tidying Template
# Author: Natalia Portales and Sydney Waloven
# Date: 2024-01-27
# Description: Source: NOAA
# Aerial Surveys of Arctic Marine Mammals (ASAMM) collected by Marine Mammal Laboratory,
# Bureau of Ocean Energy Management, and other agencies in the Bering, Chukchi,
# Beaufort Seas, and Amundsen Gulf from 1979 to 2019 and North Slope Borough, Alaska
# from 2020 to 2021
# https://www.ncei.noaa.gov/archive/archive-management-system/OAS/bin/prd/jquery/accession/download/39614

# This script follows standardized methods of tidying for the marine mammal data sets we have collected contributing to a data paper.

# Libraries and Packages --------------------------------------------------

library(terra)
library(sf)
library(tidyverse)
library(readr)
library(xml2) # to read in metadata xml file 
library(lubridate) # to separate date into year, month, day columns

# Clear history 
rm(list=ls())

# ASAMM Surveys -----------------------------------------------------------

# Import raw occurrence data
datadir <- "C:/Users/Natalia Portales/Michigan State University/Waloven, Sydney - ATP-MarineMammal-DataRaw/"
datadir <- "../../ATP-MarineMammal-DataRaw/"

# Save file path prefix
asamm <- "13_aerial_surveys_asamm/0039614/11.11/data/0-data/"

# Specify file path to Output files for relevant years

# 2012-2014
master_v0 <- st_read(paste0(datadir, asamm, "Aerial_Master_2012_2014_v0_32_Output.csv"))
# 2015-2017
master_v24 <- st_read(paste0(datadir, asamm, "Aerial_Master_2015_2017_v24_Output.csv"))
# 2018-2021
master_v2 <- st_read(paste0(datadir, asamm, "Aerial_Master_2018_2021_v2_Output.csv"))

# 2012-2014 Data ----------------------------------------------------------

# Check out column names
names(master_v0)

v0_mams <- master_v0 %>%
  
  # Drop NAs in coordinates
  drop_na(ArcLat, ArcLong) %>%
  
  # Filter to select species of interest by scientific name and spatial location within the study area bounds.
  filter(species %in% c("bowhead whale", "gray whale", "beluga", "fin whale", "humpback whale", "minke whale", "killer whale", "walrus", "bearded seal", "ringed seal"),
         decimallatitude >= 62 & decimallatitude <= 66.5, 
         decimallongitude >= -175 & decimallongitude <= -164,
         # Keep rows where the Sightcue column == body to only keep observations where the animal was ID'd by its full body
         Sightcue == "body") %>%
  
  mutate(
         
# Change values in scientificName to reflect species scientific names instead of common names
         scientificName = 
           case_when(Species == "bowhead whale" ~ "Balaena mysticetus",
                     Species == "gray whale" ~ "Eschrichtius robustus",
                     Species == "beluga" ~ "Delphinapterus leucas",
                     Species == "fin whale" ~ "Balaenoptera physalus",
                     Species == "humpback whale" ~ "Megaptera novaeangliae",
                     Species == "minke whale" ~ "Balaenoptera acutorostrata",
                     Species == "killer whale" ~ "Orcinus orca",
                     Species == "walrus" ~ "Odobenus rosmarus",
                     Species == "bearded seal" ~ "Erignathus barbatus",
                     Species == "ringed seal" ~ "Pusa hispida",
                     TRUE ~ NA),
         
         # Add columns for method of observation and source
         methodObs = "Aerial survey",
         source = "ASAMM 1979-2019") %>% 
  
  # Subset 2012-2014 dataframe to get rid of extraneous columns
  dplyr::select(
    GMT_Minus8_DateTime,
    ArcLat,
    ArcLong,
    Species,
    Sightcue,
    Totalno,
  ) %>%
  
  # Convert to sf and transform to Alaska Albers
  st_as_sf(coords = c("decimalLon", "decimalLat"),
           crs = 4326,
           remove = FALSE) %>% 
  st_transform(3338) %>% 
  
  # Eliminate repeat occurrences
  distinct() %>% 
  
  # Rearrange - this will order the data by the earliest date and then follow chronologically
  arrange(year, month, day)


