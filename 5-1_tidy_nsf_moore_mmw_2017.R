# Title: Marine Mammal Data Paper Tidying 
# Author: Natalia Portales and Sydney Waloven
# Date: 2024-03-23
# Description: # Source: Arctic Data Center. This dataset was obtained from: Arctic Data Center Marine Mammal Watch, Northern Bering Sea and Chukchi Sea August 2017. https://arcticdata.io/catalog/view/doi%3A10.18739%2FA27P8TD2J

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

# This script will tidy mmw survey data from 2017.
mmwatch_mams <- read.csv(paste0(datadir,"5_nsf_moore_mmw_2017/data/DBO_MMWatch_HLY1702_August2017_SMoore.csv"))

# data tidying ------------------------------------------------------------

# Check out column names
names(mmwatch_mams)

mams_new <- mmwatch_mams %>%
  
  # Drop NAs in coordinate, scientific name, and date columns
  drop_na(LAT..N., LONG..W., SPP, Date) %>% 
  
  # Modify and create new columns using mutate() function
  # Add method of observation and source columns
  mutate(methodObs = "Research cruise visual observation survey",
         source = "Marine Mammal Watch (Healy) Aug-Sep 2017", 
         
         # Change format of lat/lon columns from degrees-decimal minutes to decimal degrees
         deg_lat =
           as.numeric(substr(LAT..N., 1, 2)),
         degfrac_lat =
           as.numeric(substr(LAT..N., 3, 6))/60,
         
         deg_lon =
           as.numeric(substr(LONG..W., 1, 3)),
         degfrac_lon =
           as.numeric(substr(LONG..W., 4, 6))/60,
         
         decimalLat = deg_lat + degfrac_lat,
         decimalLon = deg_lon + degfrac_lon,
         
         # Make longitude column negative to reflect proper hemisphere
         decimalLon = decimalLon*(-1), 
         
         # format date column
         date_mdy = mdy(Date), # this date column is now of class "Date" like "POSIXct"
         
         # Add year, month, day columns
         year = 2017,
         month = month(date_mdy),
         day = day(date_mdy), # you can extract the year, month, or day from Date objects like this
         
         # Change values in scientificName to reflect species names instead of species code
         scientificName = 
           case_when(SPP == 1 ~ "Balaena mysticetus",
                     SPP == 2 ~ "Eschrichtius robustus",
                     SPP == 4 ~ "Balaenoptera physalus",
                     SPP == 5 ~ "Megaptera novaeangliae",
                     SPP == 6 ~ "Balaenoptera acutorostrata",
                     SPP == 8 ~ "Orcinus orca",
                     SPP == 11 ~ "Odobenus rosmarus",
                     SPP == 12 ~ "Erignathus barbatus",
                     SPP == 13 ~ "Pusa hispida",
                     SPP == 14 ~ "Phoca largha",
                     SPP == 16 ~ "Unidentified cetacean",
                     SPP == 17 ~ "Unidentified pinniped",
                     SPP == 19 ~ "Phocoenoides dalli",
                     SPP == 22 ~ "Arctocephalinae",
                     TRUE ~ NA)) %>%
  
  # Remove unneeded columns and rename
  select(scientificName,
         year,
         month,
         day,
         decimalLat,
         decimalLon,
         count = No.,
         methodObs,
         source) %>% 
  
  # Filter to select species of interest by scientific name and spatial location within the study area bounds.
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

# Save as csv into respective folder - add tidy to the end of file names so we can differentiate easily
write_csv(mams_new, "../Data_Processed/TidyScripts/5-1_tidy_nsf_moore_mmw_2017.csv")

# Read in tidied data
# marine_mams <- read_csv("../Data_Processed/TidyScripts/5-1_tidy_nsf_moore_mmw_2017.csv")

