# Title: Marine Mammal Data Paper Tidying 
# Author: Natalia Portales and Sydney Waloven
# Date: 2024-03-05
# Description:  Source: Arctic Data Center. This dataset was obtained from Marine Mammal Sightings from cruises in 2018. https://arcticdata.io/catalog/view/doi%3A10.18739%2FA2NV99B09
# Part 1

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

# This script will tidy the first data set of mmw survey data from 2018.
mmwatch_mams <- read.csv(paste0(datadir, "7_nsf_stafford_mmw_2018/data/DBO_SWL_2018.csv"))

# data tidying ------------------------------------------------------------

# Check out column names
names(mmwatch_mams)

mams_new <- mmwatch_mams %>%
  
  # Drop NAs in coordinates, scientific name, and date columns
  drop_na(LAT..N., LONG..W., SPP, Date) %>% 
  
  # Modify and create new columns using mutate() function
  # Add method of observation and source columns
  mutate(methodObs = "Research cruise visual observation survey",
         source = "Marine Mammal Watch Cruises (SWL) 2018",
         
        # Make longitude column negative to reflect proper hemisphere
        LONG..W. = LONG..W.*(-1), 
         
        # Format date column
        date_mdy = mdy(Date), # This date column is now of class "Date" like "POSIXct"

        # Add columns for year, month, day
        year = 2018,
        month = 7,
        day = day(date_mdy), # you can extract the year, month, or day from Date objects like this
        
       # Change values in scientificName to reflect species names instead of species code
       scientificName = 
         case_when(SPP == 2 ~ "Eschrichtius robustus",
                   SPP == 4 ~ "Balaenoptera physalus",
                   SPP == 5 ~ "Megaptera novaeangliae",
                   SPP == 6 ~ "Balaenoptera acutorostrata",
                   SPP == 11 ~ "Odobenus rosmarus",
                   SPP == 14 ~ "Phoca largha",
                   SPP == 19 ~ "Phocoenoides dalli",
                   SPP == 22 ~ "Callorhinus ursinus",
                   TRUE ~ NA)) %>%
  
  # Remove unneeded columns and rename
  select(scientificName,
         year,
         month,
         day,
         decimalLat = LAT..N.,
         decimalLon = LONG..W.,
         count = NO,
         methodObs,
         source) %>%
  
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
write_csv(mams_new, "../Data_Processed/TidyScripts/7-1_nsf_dbo_laurier_2018.csv", append = FALSE)

# Save rds and shps
# saveRDS(mams_new, "../Data_Processed/rds_and_shps_new_tidy/7-1_nsf_dbo_laurier_2018.rds")
# st_write(mams_new, "../Data_Processed/rds_and_shps_new_tidy/7-1_nsf_dbo_laurier_2018.shp", append = FALSE)
  