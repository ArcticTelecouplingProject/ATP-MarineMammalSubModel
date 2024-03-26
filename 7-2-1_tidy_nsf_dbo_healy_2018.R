# Title: Marine Mammal Data Paper Tidying 
# Author: Natalia Portales and Sydney Waloven
# Date: 2024-03-18
# Description: Source: Arctic Data Center. This dataset was obtained from Marine Mammal Sightings from cruises in 2018. https://arcticdata.io/catalog/view/doi%3A10.18739%2FA2NV99B09
# Part II

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

# This script will tidy the second data set of mmw survey data from 2018.
mmwatch_mams <- read.csv(paste0(datadir,"7_nsf_stafford_mmw_2018/data/Healy_DBONCIS_Aug2018_byDBOregion.csv"))

# data tidying ------------------------------------------------------------

# Check out column names
names(mmwatch_mams)

mams_new <- mmwatch_mams %>%
  
  # Drop NAs in coordinates and scientific name
  drop_na(LAT, LONG, SPP, DATE) %>% 
  
  # Add method of observation and source columns
  mutate(methodObs = "Research cruise visual observation survey",
         source = "Marine Mammal Watch Cruises (Healy) 2018", 
         
         # format date column
         date_mdy = mdy(DATE), # this date column is now of class "Date" like "POSIXct"
         
         # Add columns for year, month, day
         year = 2018,
         month = 8,
         day = day(date_mdy), # you can extract the year, month, or day from Date objects like this
         
         # Change values in scientificName to reflect species names instead of species code
         scientificName = 
           case_when(SPP == 1 ~ "Balaena mysticetus",
                     SPP == 2 ~ "Eschrichtius robustus",
                     SPP == 4 ~ "Balaenoptera physalus",
                     SPP == 5 ~ "Megaptera novaeangliae",
                     SPP == 6 ~ "Balaenoptera acutorostrata",
                     SPP == 10 ~ "Ursus maritimus",
                     SPP == 11 ~ "Odobenus rosmarus",
                     SPP == 12 ~ "Erignathus barbatus",
                     SPP == 13 ~ "Pusa hispida",
                     SPP == 14 ~ "Phoca largha",
                     SPP == 15 ~ "Histriophoca fasciata",
                     SPP == 16 ~ "Unidentified cetacean",
                     SPP == 17 ~ "Unidentified pinniped",
                     TRUE ~ NA)) %>%
  
  # Remove unneeded columns and rename
  select(scientificName,
         year,
         month,
         day,
         decimalLat = LAT,
         decimalLon = LONG,
         count = NO,
         methodObs,
         source) %>% 
  
  # Convert to sf and transform to Alaska Albers
  st_as_sf(coords = c("decimalLon", "decimalLat"),
           crs = 4326,
           remove = FALSE) %>% 
  st_transform(3338) %>% 
  
  # Filter to select species of interest by scientific name and spatial location within the study area bounds.
  filter(decimalLat >= 46.08836 & decimalLat <= 73.92759, 
         decimalLon >= 172.18938 | decimalLon <= -146.30217) %>%
  
  # Eliminate repeat occurrences
  distinct() %>% 
  
  # Rearrange - this will order the data by the earliest date and then follow chronologically
  arrange(year, month, day)

# Save as csv into respective folder - add tidy to the end of file names so we can differentiate easily
write_csv(mams_new, "../Data_Processed/7-2_nsf_dbo_healy_2018/7-2-1_nsf_dbo_healy_2018_tidy.csv")

