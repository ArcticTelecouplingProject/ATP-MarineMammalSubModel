# Title: Marine Mammal Data Paper Tidying 
# Author: Natalia Portales and Sydney Waloven
# Date: 2024-03-18
# Description: Arctic Data Center "Marine Mammal sighting data from cruises in the Pacific Arctic, 2018, from Distributed Biological Observatory (DBO) regions" https://arcticdata.io/catalog/view/doi%3A10.18739%2FA2NV99B09
# Part 2

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

# This script will tidy the second data set of mmw survey data from 2018.
mmwatch_mams <- read.csv(paste0(datadir, "07_nsf_stafford_mmw_2018/data/Healy_DBONCIS_Aug2018_byDBOregion.csv"))

# data tidying ------------------------------------------------------------

# Check out column names
names(mmwatch_mams)

mams_new <- mmwatch_mams %>%
  
  # Drop NAs in coordinates, scientific name, and date columns
  drop_na(LAT, LONG, SPP, DATE) %>% 
  
  # Modify and create new columns using mutate() function
  # Add method of observation and source columns
  mutate(methodObs = "Research cruise visual observation",
         source = "Marine Mammal Watch Cruises (Healy) Aug 2018", 
         
         # Format date column
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
                     TRUE ~ NA),
         
         # Add column to locate where the data was downloaded from
         dataOrigin = "NSF Arctic Data Center") %>%
  
  # Remove unneeded columns and rename
  select(scientificName,
         year,
         month,
         day,
         decimalLat = LAT,
         decimalLon = LONG,
         count = NO,
         methodObs,
         source,
         dataOrigin) %>% 
  
  # Filter to select species of interest within the study area bounds
  filter(scientificName %in% c("Odobenus rosmarus", "Phoca largha", "Erignathus barbatus"),
         decimalLat >= 46.08836 & decimalLat <= 73.92759, 
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

# Save as csv into respective folder
write_csv(mams_new, "../Data_Processed/TidyScripts/07-2_nsf_dbo_healy_2018.csv", append = FALSE)

# Save rds and shps
# saveRDS(mams_new, "../Data_Processed/rds_and_shps_new_tidy/7-2_nsf_dbo_healy_2018.rds")
# st_write(mams_new, "../Data_Processed/rds_and_shps_new_tidy/7-2_nsf_dbo_healy_2018.shp", append = FALSE)

