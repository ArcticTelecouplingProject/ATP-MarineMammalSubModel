# Title: Arctic Species Trend Index (ASTI): Marine
# Author: Sydney Waloven
# Date: 2024-04-05
# Description: Data obtained from: https://obis.org/dataset/19119a0f-92f3-4662-85c5-30f8472067cf
#
# The Circumpolar Biodiversity Monitoring Program, a cornerstone programme of the Conservation of Arctic Flora and Fauna (CAFF), Arctic Council working Group is an international network of scientists, government agencies, Indigenous organizations and conservation groups working together to harmonize and integrate efforts to monitor the Arctic's living resources.CBMP experts are developing four coordinated and integrated Arctic Biodiversity Monitoring Plans to help guide circumpolar monitoring efforts. Results will be channeled into effective conservation, mitigation and adaptation policies supporting the Arctic. These plans represent the Arctic's major ecosystems(Marine, Freshwater, Coastal, Terrestrial).
# 
# It is important that monitoring programs develop the most effective reporting strategies if they are to inform decision making. To facilitate effective and consistent reporting, the CBMP has chosen a suite of indices and indicators that provide a comprehensive picture of the state of Arctic biodiversity – from species to habitats to ecosystem processes to ecological services. These indices and indicators are developed in a hierarchical manner, allowing users to drill down into the data from the higher-order indices to more detailed indicators. These are being developed through an expert consultation process.
# 
# The Arctic Species Trend Index (ASTI) is part of this suite of indicators and indices developed by CAFFs CBMP. It tracks trends in over 300 Arctic vertebrate species and comprises the Arctic component of the Living Planet Index. It is important to identify how wildlife and ecosystems are changing in order to develop effective conservation and adaptation strategies in the Arctic, an environment undergoing dramatic changes. The ASTI describes overall trends across species, taxonomy, ecosystems, regions and other categories.

# Data number 9. Tidy script number 15.


# setup -------------------------------------------------------------------

# Load libraries
library(sf)
library(tidyverse)

# Clear environment history 
rm(list = ls())

# Set datadir
datadir <- "C:/Users/Natalia Portales/Michigan State University/Waloven, Sydney - ATP-MarineMammal-DataRaw/"
datadir <- "../../ATP-MarineMammal-DataRaw/"

# Read in csv
asti <- read_csv(paste0(datadir, "15_asti_caff_obis/9_asti_caff_obis.csv"))


# data tidying ------------------------------------------------------------

# Check out column names
names(asti)

# Look at what species are in this data set
asti$species %>% unique()
# Only species of interest that it contains for us is the walrus

walrus <- asti %>% 
  
  # Drop NAs in coordinates
  drop_na(decimalLatitude, decimalLongitude) %>% 
  
  # Remove unneeded columns and rename
  select(scientificName = verbatimScientificName,
         date = eventDate,
         decimalLat = decimalLatitude,
         decimalLon = decimalLongitude,
         count = individualCount,
         methodObs = basisOfRecord) %>% 
  
  # Subset to species of interst
  filter(scientificName == "Odobenus rosmarus") %>% 
  
  # Standardize what's in the methodObs column and add source column
  mutate(methodObs = "Human observation",
         source = "Arctic Species Trend Index: Marine",
         
         # Add columns for year, month, day - it looks like these occurrences were defaulted to Jan 1 for each one so I've replaced them with NAs
         year = as.numeric(substr(date, 5, 8)),
         month = NA,
         day = NA,
         count = 1) %>% 
  
  # Remove old date column
  select(scientificName,
         year,
         month, 
         day,
         decimalLat,
         decimalLon,
         count,
         methodObs,
         source) %>% 
  
  # Remove occurrences outside of the study bounds
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
  arrange(year)


# Save as csv
write_csv(walrus, "../Data_Processed/15_asti_marine_walrus.csv")

# Save as rds
saveRDS(walrus, "../Data_Processed/15_asti_marine_walrus.rds")

# Save as shp
st_write(walrus, "../Data_Processed/15_asti_marine_walrus.shp")
