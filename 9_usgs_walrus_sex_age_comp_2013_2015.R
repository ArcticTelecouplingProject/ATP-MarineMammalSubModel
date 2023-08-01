# 10Jan2023
# Author: Sydney Waloven and Natalia Portales

# Source: USGS
# This dataset was obtained from Sex and Age Composition of Walrus Groups Hauled
# Out on Ice Floes in the Bering and Chukchi Seas 2013-2015
# https://alaska.usgs.gov/products/data.php?dataid=354

# Species isolated in this dataset: walrus

# This dataset was tidied and ready for data integration on 01Aug2023
# Last edited: 01Aug2023


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

usgs_walrus_comp <- read.csv(paste0(datadir, "9_usgs_walrus_sex_age_comp_2013_2015/walrus_hauloutGroupComp_battaile_2013_2015.csv"))

head(usgs_walrus_comp)

# Create a dataframe to generate metadata summaries on the occurrence data at various steps of tidying
# OccurrenceSummaries <- data.frame(name = "usgs_walrus_comp") 
# OccurrenceSummaries$before <- length(usgs_walrus_comp$GroupSize)

# Add to metadata dataframe for summaries on the occurrence data at various steps of tidying
# OccurrenceSummaries$isolatedSpecies <- length(usgs_walrus_comp$GroupSize)

# Remove extraneous columns of info 
colnames(usgs_walrus_comp)
usgs_walrus <- usgs_walrus_comp %>% dplyr::select(Year,
                                           DateTimeStart,
                                           GroupSize,
                                           Latitude,
                                           Longitude,
                                           GroupSizingMethod)
## GroupSizingMethod does not need to be included in the saved output this time but
## I saved it for easy backtracking through the script to include it if needed.
## Might be necessary to revisit for potential error source. SW 01Aug2023

# Add to metadata dataframe for summaries on the occurrence data at various steps of tidying
# OccurrenceSummaries$columnsofInterest <- length(usgs_walrus$GroupSize)


# Set CRS -----------------------------------------------------------------

# Check the CRS
st_crs(usgs_walrus) # this is non-spatial so the CRS is NA

# Remove NAs from latitude and longitude columns
usgs_walrus_new <- usgs_walrus[!is.na(usgs_walrus$Latitude), ]

# Convert data to a spatial object with CRS 4326 (since that is regular lat/lon) - this is still unprojected
usgs_walrus_sp <- st_as_sf(usgs_walrus_new, coords = c("Longitude", "Latitude"), crs = 4326, remove=FALSE) 

# Project the data to Alaska Albers 3338
walrus_sp <- usgs_walrus_sp %>% st_transform(crs = 3338)


# Modify columns ----------------------------------------------------------

# Add scientificName and commonName columns
usgs_walrus$scientificName <- "Odobenus rosmarus divergens"
usgs_walrus$commonName <- "Walrus"

# Add a new column to specify data source so you can backtrack later if needed
walrus_sp$source <- "USGS Sex and Age Comp of Walrus Haulouts 2013-2015"

# May also want to add other columns that help add information about the data source
# (e.g., vessel based, plane based, etc.)

# Add a new column to specify the method of data collection used
walrus_sp$methodObservation <- "Research vessel visual observation"

# Separate DateTimeStart into month and day cols by using substr()
walrus_sp$month <- substr(walrus_sp$DateTimeStart, 6, 7)
walrus_sp$day <- substr(walrus_sp$DateTimeStart, 9, 10)

# Remove extraneous columns again, rearrange, and name
walrus_sp2 <- walrus_sp %>% dplyr::select(scientificName,
                                      commonName,
                                      Year,
                                      month,
                                      day,
                                      GroupSize,
                                      Latitude,
                                      Longitude,
                                      methodObservation,
                                      source) %>% 
  rename(scientificName = scientificName,
         year = Year,
         groupSize = GroupSize,
         decimalLat = Latitude,
         decimalLon = Longitude)


# Eliminate repeats -------------------------------------------------------

# number of occurrences before eliminating repeats
# nrow(walrus_sp2)

# Eliminate repeat occurrences
unique_walrus <- distinct(walrus_sp2)

# number of occurrences after eliminating repeats
# nrow(unique_walrus)

# Remove NAs
unique_walrus_omit <- na.omit(unique_walrus)

# Add to metadata dataframe for summaries on the occurrence data at various steps of tidying
OccurrenceSummaries$uniqueOccurrences <- length(unique_walrus_omit$groupSize)

View(OccurrenceSummaries)

## Tada now you have a clean and tidy dataset to work with and merge

View(unique_walrus_omit) # save this

# Can save this data as an output as both shp and csv
# st_write(unique_walrus_omit, "../Data_Processed/9_usgs_walrus_sex_age_comp_2013_2015/9_usgs_walrus_sex_age_comp_2013_2015.csv")

# Save metadata dataframe
# write.csv(OccurrenceSummaries, "../Data_Processed/Metadata/Metadata_9_usgs_walrus_sex_age_comp_2013_2015.csv")
