# Title: Marine Mammal Data Paper Tidying Template
# Author: Sydney Waloven
# Date: 2024-01-08
# Description: This script is meant to serve as a template for tidying the marine mammal data sets we have collected to standardize the methods and create a data paper.

# Remember to check "Soft-wrap R source files" in Global Options.


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
nmml <- read_csv(paste0(datadir, "8_nmml_aerial_1979_2010/8_nmml_aerial_1979_2010.csv"))


# data tidying ------------------------------------------------------------

# Check out column names
names(nmml)

marine_mams <- nmml %>%
  
  # Drop NAs in coordinates
  drop_na(decimalLatitude, decimalLongitude) %>% 
  
  # Remove unneeded columns and rename
  # Order of columns here determines order of columns in df. Can also rename columns in this function (new name on left, old name on right).
  select(scientificName = species,
         year,
         month,
         day,
         decimalLat = decimalLatitude,
         decimalLon = decimalLongitude,
         count = individualCount) %>% 
  
  # Subset to species of interest or comment out to see data for all species
  # filter(scientificName == c("Erignathus barbatus", "Phoca largha", "Odobenus rosmarus")) %>%
  
  # Add method of observation and source columns
  mutate(methodObs = "Aerial survey",
         source = "NMML Marine Mammal Aerial Surveys 1979-2010") %>% 
  
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
write_csv(marine_mams, "../Data_Processed/8_nmml_aerial_1979_2010/8_nmml_aerial_tidy.csv")

# Read in tidied data
# marine_mams <- read_csv("../Data_Processed/8_nmml_aerial_tidy.csv")

# Not all data sets will be the same and different functions may be needed to further tidy the data all into the same format. See Example 2.


# Example 2 ---------------------------------------------------------------

# This example requires more tidying steps because we need to convert latitude and longitude data from degrees-decimal minutes to decimal degrees. 

# Import data - 4_nsf_moore_mmw_2016
datadir <- "C:/Users/Natalia Portales/Michigan State University/Waloven, Sydney - ATP-MarineMammal-DataRaw/"
datadir <- "../../ATP-MarineMammal-DataRaw/"

mmwatch2016 <- read_csv(paste0(datadir, "4_nsf_moore_mmw_2016/data/DBO_MMWatch_SWL2016_MooreGrebmeierVagle.csv"))

# Check out column names
names(mmwatch2016)

mmwatch_mams <- mmwatch2016 %>%
  
  # Drop NAs in coordinates
  drop_na(LAT_N, LONG_W, SPP) %>% 
  
  # Convert columns from degrees-decimal minutes to decimal-degrees
  # substr() takes the column name, starting position, and end length of the string as arguments
  mutate(deg_lat =
           as.numeric(substr(LAT_N, 1, 2)),
         degfrac_lat =
           as.numeric(substr(LAT_N, 3, 6))/60,
         
         deg_lon =
           as.numeric(substr(LONG_W, 1, 3)),
         degfrac_lon =
           as.numeric(substr(LONG_W, 4, 6))/60,
         
         decimalLat = deg_lat + degfrac_lat,
         decimalLon = deg_lon + degfrac_lon,
         
         # Add columns for year, month, day
         year = 2016,
         month = 7,
         day = as.numeric(substr(Date, 1, 2)),
         
         # Change values in scientificName to reflect species names instead of species code
         scientificName = 
           case_when(SPP == 1 ~ "Balaena mysticetus",
                     SPP == 2 ~ "Eschrichtius robustus",
                     SPP == 3 ~ "Delphinapterus leucas",
                     SPP == 4 ~ "Balaenoptera physalus",
                     SPP == 5 ~ "Megaptera novaeangliae",
                     SPP == 6 ~ "Balaenoptera acutorostrata",
                     SPP == 7 ~ "Eubalaena glacialis",
                     SPP == 8 ~ "Orcinus orca",
                     SPP == 9 ~ "Phocoena phocoena",
                     SPP == 10 ~ "Ursus maritimus",
                     SPP == 11 ~ "Odobenus rosmarus",
                     SPP == 12 ~ "Erignathus barbatus",
                     SPP == 13 ~ "Pusa hispida",
                     SPP == 14 ~ "Phoca largha",
                     SPP == 15 ~ "Histriophoca fasciata",
                     SPP == 16 ~ "Unidentified cetacean",
                     SPP == 17 ~ "Unidentified pinniped",
                     SPP == 18 ~ "Physeter macrocephalus",
                     SPP == 19 ~ "Phocoenoides dalli",
                     SPP == 20 ~ "Lagenorhynchus obliquidens",
                     SPP == 21 ~ "Eumetopias jubatus",
                     SPP == 22 ~ "Arctocephalinae",
                     SPP == 23 ~ "Phoca vitulina",
                     SPP == 24 ~ "Lissodelphis borealis",
                     TRUE ~ NA),
         
         # Add columns for method of observation and source
         methodObs = "Marine Mammal Watch July 2016",
         source = "Research cruise visual observation survey") %>% 
  
  # Subset to species of interest or comment out to see data for all species
  # filter(scientificName == c("Erignathus barbatus", "Phoca largha", "Odobenus rosmarus")) %>%
  
  # Remove unneeded columns and rename
  select(scientificName,
         year,
         month,
         day,
         decimalLat,
         decimalLon,
         count = No.,) %>% 
  
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
write_csv(mmwatch2016, "../Data_Processed/4_nsf_moore_mmw_2016/4_nsf_moore_mmw_tidy.csv")

# Read in tidied data
# marine_mams <- read_csv("../Data_Processed/8_nmml_aerial_tidy.csv")
