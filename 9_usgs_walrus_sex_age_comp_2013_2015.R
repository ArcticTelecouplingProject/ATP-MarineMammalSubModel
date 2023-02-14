# 10Jan2023
# Author: Sydney Waloven

# Source: USGS
# This dataset was obtained from Sex and Age Composition of Walrus Groups Hauled
# Out on Ice Floes in the Bering and Chukchi Seas 2013-2015
# https://alaska.usgs.gov/products/data.php?dataid=354

# Species isolated in this dataset: walrus

# This dataset was tidied and ready for data integration on 10Jan2023
# Last edited: 13Feb2023


# Libraries and Packages --------------------------------------------------

library(terra)
library(sf)
library(tidyverse)

# Clear history 
rm(list=ls())


# USGS Walrus Sex and Age 2013-2015 ---------------------------------------

# Import raw walrus occurrence data

datadir <- "C:/Users/Natalia Portales/Michigan State University/Waloven, Sydney - ATP-MarineMammal-DataRaw/"
datadir <- "../../ATP-MarineMammal-DataRaw/"

usgs_walrus_comp <- read.csv(paste0(datadir,"9_usgs_walrus_sex_age_comp_2013_2015/walrus_hauloutGroupComp_battaile_2013_2015.csv"))

head(usgs_walrus_comp)

# Remove extraneous columns of info 
colnames(usgs_walrus_comp)
usgs_walrus <- usgs_walrus_comp %>% dplyr::select(Year,
                                           DateTimeStart,
                                           GroupSize,
                                           Latitude,
                                           Longitude,
                                           GroupSizingMethod)


# Convert to spatial df ---------------------------------------------------

# Add scientificName and commonName columns
usgs_walrus$scientificName <- "Odobenus rosmarus divergens"
usgs_walrus$commonName <- "Walrus"

# Save lat/lon columns info into their own df to add in again later on
# or else they will disappear when converting to spatial object with st_as_sf
usgs_latlon <- usgs_walrus %>% dplyr::select(scientificName,
                                                Latitude,
                                                Longitude,
                                                DateTimeStart)

# Check the CRS
st_crs(usgs_walrus) # this is non-spatial so the CRS is NA

# Remove NAs from latitude and longitude columns
usgs_walrus_new <- usgs_walrus[!is.na(usgs_walrus$Latitude), ]

# Convert data to a spatial object with CRS 4326 (since that is regular lat/lon) - this is still unprojected
usgs_walrus_sp <- st_as_sf(usgs_walrus_new, coords = c("Longitude", "Latitude"), crs = 4326) 

# Project the data to Alaska Albers 3338
usgs_walrus_sp <- usgs_walrus_sp %>% st_transform(crs = 3338)

# Add back in lat/lon cols using left join to merge lat/lon data onto spatial data rather than spatial data onto lat/lon data
walrus_sp <- usgs_walrus_sp %>% left_join(usgs_latlon, by = "DateTimeStart")

# Add a new column to specify data source so you can backtrack later if needed
walrus_sp$source <- "USGS Sex and Age Comp of Walrus Haulouts 2013-2015"
# May also want to add other columns that help add information about the data source (e.g., vessel based, plane based, etc.)
## This is why I saved the GroupSizingMethod column - does not need to be included in the saved output this time but is there
## for easy backtracking through the script to include it if needed. SW10Jan2023

walrus_sp$methodOfObservation <- "Research cruise visual observation survey"

# Separate DateTimeStart into month and day cols by using substr()
walrus_sp$month <- substr(walrus_sp$DateTimeStart, 6, 7)
walrus_sp$day <- substr(walrus_sp$DateTimeStart, 9, 10)

# Remove extraneous columns again, rearrange, and name
walrus_sp2 <- walrus_sp %>% dplyr::select(scientificName.x,
                                      commonName,
                                      Year,
                                      month,
                                      day,
                                      GroupSize,
                                      Latitude,
                                      Longitude,
                                      methodOfObservation,
                                      source) %>% 
  rename(scientificName = scientificName.x,
         year = Year,
         groupSize = GroupSize,
         decimalLat = Latitude,
         decimalLon = Longitude)


## Tada now you have a clean and tidy dataset to work with and merge

View(walrus_sp2)

# Can save this data as an output as both shp and csv
# st_write(walrus_sp2, "../Data_Processed/9_usgs_walrus_sex_age_comp_2013_2015/9_usgs_walrus_sex_age_comp_2013_2015.shp")
