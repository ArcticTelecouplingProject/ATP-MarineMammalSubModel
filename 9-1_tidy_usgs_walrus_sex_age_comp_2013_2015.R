# Title: Marine Mammal Data Paper Tidying 
# Author: Natalia Portales and Sydney Waloven
# Date: 2024-01-15
# Description:  Source: USGS. This dataset was obtained from Sex and Age Composition of Walrus Groups Hauled Out on Ice Floes in the Bering and Chukchi Seas 2013-2015. https://alaska.usgs.gov/products/data.php?dataid=354

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

# This script will tidy USGS visual surveys from 2013-2015.
usgs_walrus_comp <- read_csv(paste0(datadir, "9_usgs_walrus_sex_age_comp_2013_2015/walrus_hauloutGroupComp_battaile_2013_2015.csv"))

# data tidying ------------------------------------------------------------

# Check out column names
names(usgs_walrus_comp)

usgs_walrus <- usgs_walrus_comp %>%
  
  # Drop NAs in coordinate and year columns
  drop_na(Latitude, Longitude, Year) %>% 
  
  # Modify and create new columns using mutate() function 
  # Separate DateTimeStart into month and day cols by using substr()
  mutate(month = substr(DateTimeStart, 6, 7),
         day = substr(DateTimeStart, 9, 10),
         
         # Add scientific name column
         scientificName = "Odobenus rosmarus",
         
         # Add method of observation and source columns
         methodObs = "Research cruise visual observation survey",
         source = "USGS Sex and Age Comp of Walrus Haulouts 2013-2015") %>% 
  
  # Remove unneeded columns and rename
  select(scientificName,
         year = Year,
         month,
         day,
         decimalLat = Latitude,
         decimalLon = Longitude,
         count = GroupSize,
         methodObs,
         source) %>% 
  
  # Filter to select occurrences by spatial location within the study area bounds.
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
write_csv(usgs_walrus, "../Data_Processed/TidyScripts/9-1_tidy_usgs_walrus_sex_age_comp_2013_2015.csv")

# Save rds and shps
saveRDS(usgs_walrus, "../Data_Processed/rds_and_shps_new_tidy/9-1_tidy_usgs_walrus_sex_age_comp_2013_2015.rds")
st_write(usgs_walrus, "../Data_Processed/rds_and_shps_new_tidy/9-1_tidy_usgs_walrus_sex_age_comp_2013_2015.shp")

# Read in tidied data
# usgs_walrus <- read_csv("../Data_Processed/TidyScripts/9-1_tidy_usgs_walrus_sex_age_comp_2013_2015.csv")
