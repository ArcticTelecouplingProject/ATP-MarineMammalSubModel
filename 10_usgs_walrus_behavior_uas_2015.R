# 10Jan2023
# Author: Sydney Waloven

# Source: USGS
# This dataset was obtained from Behavior of Pacific Walruses Hauled Out on Sea Ice
# During UAS Overflights, Eastern Chukchi Sea 2015
# https://alaska.usgs.gov/products/data.php?dataid=74

# Species isolated in this dataset: walrus

# This dataset was tidied and ready for data integration on 10Jan2023
# Last edited: 10Jan2023


# Libraries and Packages --------------------------------------------------

library(terra)
library(sf)
library(tidyverse)

# Clear history 
rm(list=ls())



# USGS Walrus UAS Overflights 2015 ----------------------------------------

# Import raw walrus occurrence data
datadir <- "C:/Users/Natalia Portales/Michigan State University/Waloven, Sydney - ATP-MarineMammal-DataRaw/"

usgs_walrus_uas <- read.csv(paste0(datadir,"10_usgs_walrus_behavior_uas_2015/walrus_environConditionsUAS_chukchi_battaile_2015.csv"))

head(usgs_walrus_uas)

# Remove extraneous columns of info 
colnames(usgs_walrus_uas)
usgs_walrus <- usgs_walrus_uas %>% dplyr::select(Flight,
                                                 Group_Size,
                                                 Latitude,
                                                 Longitude)


# Convert to spatial df ---------------------------------------------------

# Add scientificName and commonName columns
usgs_walrus$scientificName <- "Odobenus rosmarus divergens"
usgs_walrus$commonName <- "Walrus"

# Save lat/lon columns info into their own df to add in again later on
# or else they will disappear when converting to spatial object with st_as_sf
usgs_latlon <- usgs_walrus %>% dplyr::select(scientificName,
                                             Latitude,
                                             Longitude,
                                             Flight)

# Check the CRS
st_crs(usgs_walrus) # this is non-spatial so the CRS is NA

# Convert data to a spatial object with CRS 4326 (since that is regular lat/lon) - this is still unprojected
usgs_walrus_sp <- st_as_sf(usgs_walrus, coords = c("Longitude", "Latitude"), crs = 4326) 

# Project the data to Alaska Albers 3338
usgs_walrus_sp <- usgs_walrus_sp %>% st_transform(crs = 3338)

# Add back in lat/lon cols using left join to merge lat/lon data onto spatial data rather than spatial data onto lat/lon data
walrus_sp <- usgs_walrus_sp %>% left_join(usgs_latlon, by = "Flight")

# Add a new column to specify data source so you can backtrack later if needed
walrus_sp$source <- "USGS Walrus Behavior UAS 2015"
# May also want to add other columns that help add information about the data source (e.g., vessel based, plane based, etc.)

# walrus_sp$methodOfObservation <- "visual observations"

# Separate DateTimeStart into month and day cols by using substr()
walrus_sp$year <- 2015
walrus_sp$month <- substr(walrus_sp$Flight, 5, 6)
walrus_sp$day <- substr(walrus_sp$Flight, 7, 8)

# Remove extraneous columns again, rearrange, and name
walrus_sp2 <- walrus_sp %>% dplyr::select(scientificName.x,
                                          commonName,
                                          year,
                                          month,
                                          day,
                                          Group_Size,
                                          Latitude,
                                          Longitude,
                                          source) %>% 
  rename(scientificName = scientificName.x,
         groupSize = Group_Size,
         decimalLat = Latitude,
         decimalLon = Longitude)


## Tada now you have a clean and tidy dataset to work with and merge

View(walrus_sp2)

# Can save this data as an output as both shp and csv
st_write(walrus_sp2, "../Data_Processed/10_usgs_walrus_behavior_uas_2015.shp")
