# 09Jan2023
# Author: Sydney Waloven

# Source: Arctic Data Center
# This dataset was obtained from Marine Mammal aerial surveys in the Bering, Chukchi,
# and Beaufort Seas 1979-2010.
# https://www.gbif.org/dataset/e80fa6fc-5468-4ee7-8a74-e6dea5ccfaf9#description

# Species isolated in this dataset: walrus, bearded seal, spotted seal

# This dataset was tidied and ready for data integration on 09Jan2023
# Last edited: 12Feb2023



# Libraries and Packages --------------------------------------------------

library(terra)
library(sf)
library(tidyverse)


# Clear history 
rm(list=ls())




# NMML 1979-2010 ----------------------------------------------------------


# Read in the data
datadir <- "C:/Users/Natalia Portales/Michigan State University/Waloven, Sydney - ATP-MarineMammal-DataRaw/"
datadir <- "../../ATP-MarineMammal-DataRaw/"

nmml_aerial <- read.csv(paste0(datadir,"8_nmml_aerial_1979_2010/8_nmml_aerial_1979_2010.csv"))
head(nmml_aerial)

# Isolate out species of interest
unique(nmml_aerial$verbatimScientificName)
mmwatch_mams <- nmml_aerial[nmml_aerial$verbatimScientificName %in% c("Odobenus rosmarus", "Phoca largha", "Erignathus barbatus"), ]

# Remove extraneous columns of info 
colnames(mmwatch_mams)
mams_new <- mmwatch_mams %>% dplyr::select(verbatimScientificName,
                                           year,
                                           month,
                                           day,
                                           eventDate,
                                           decimalLatitude,
                                           decimalLongitude)


# Convert to spatial df ---------------------------------------------------

# Save lat/lon columns info into their own df to add in again later on
# or else they will disappear when converting to spatial object with st_as_sf
mmwatch_latlon <- mams_new %>% dplyr::select(verbatimScientificName,
                                             decimalLatitude,
                                             decimalLongitude,
                                             eventDate)

# Check the CRS
st_crs(mams_new) # this is non-spatial so the CRS is NA

# Convert data to a spatial object with CRS 4326 (since that is regular lat/lon) - this is still unprojected
mams_new_sp <- st_as_sf(mams_new, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326) 

# Project the data to Alaska Albers 3338
mams_new_sp <- mams_new_sp %>% st_transform(crs = 3338)

# Add back in lat/lon cols using left join to merge lat/lon data onto spatial data rather than spatial data onto lat/lon data
mams_sp <- mams_new_sp %>% left_join(mmwatch_latlon, by = "eventDate")

# Add a new column to specify data source so you can backtrack later if needed
mams_sp$source <- "NMML Marine Mammal Aerial Surveys 1979-2010"
# May also want to add other columns that help add information about the data source (e.g., vessel based, plane based, etc.)
# Add a new column to specify the method of data collection used
mams_sp$methodObservation <- "Aerial survey"

# Add a groupSize column for data integration consistency
mams_sp$groupSize <- 1 # filled with 1 because I believe all observation records are observations of 1 occurrence

# Add commonName column
mams_sp$commonName[mams_sp$verbatimScientificName.x == "Odobenus rosmarus"] <- "Walrus"
mams_sp$commonName[mams_sp$verbatimScientificName.x == "Erignathus barbatus"] <- "Bearded Seal"
mams_sp$commonName[mams_sp$verbatimScientificName.x == "Phoca largha"] <- "Spotted Seal"

# Remove extraneous columns again, rearrange, and name
mams_sp2 <- mams_sp %>% dplyr::select(verbatimScientificName.x,
                                     commonName,
                                     year,
                                     month,
                                     day,
                                     groupSize,
                                     decimalLatitude,
                                     decimalLongitude,
                                     methodObservation,
                                     source) %>% 
  rename(scientificName = verbatimScientificName.x,
         decimalLat = decimalLatitude,
         decimalLon = decimalLongitude)


## Tada now you have a clean and tidy dataset to work with and merge

View(mams_sp2)

# Can save this data as an output as both shp and csv
st_write(mams_sp2, "../Data_Processed/8_nmml_aerial_1979_2010/8_nmml_aerial_1979_2010.csv")
