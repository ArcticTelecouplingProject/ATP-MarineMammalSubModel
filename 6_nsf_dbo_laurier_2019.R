# 08Jan2023
# Author: Sydney Waloven

# Source: Arctic Data Center
# This dataset was obtained from Marine Mammal Sightings During Sir Wilfrid Laurier
# Cruise in July 2019
# https://arcticdata.io/catalog/view/doi%3A10.18739%2FA2DZ0320P

# Species isolated in this dataset: walrus, bearded seal, spotted seal

# This dataset was tidied and ready for data integration on 08Jan2023
# Last edited: 08Jan2023



# Libraries and Packages --------------------------------------------------

library(terra)
library(sf)
library(tidyverse)


# Clear history 
rm(list=ls())


# DBO Laurier 2019 --------------------------------------------------------


# Read in the data
dbo_swl_2019 <- read.csv("../../ATP-MarineMammal-DataRaw/6_nsf_dbo_laurier_2019/data/DBO_SWL_2019.csv")
head(dbo_swl_2019)

# Isolate out species of interest (see metadata for more details)
# SPP corresponds to species
# 11 = walrus
# 12 = bearded seal
# 14 = spotted seal
mmwatch_mams <- dbo_swl_2019[dbo_swl_2019$SPP %in% c(11, 12, 14), ]

# Remove extraneous columns of info 
colnames(mmwatch_mams)
mams_new <- mmwatch_mams %>% dplyr::select(SPP,
                                           Date,
                                           LAT..N.,
                                           LONG..W.,
                                           NO)


# Convert to spatial df ---------------------------------------------------


# Save lat/lon columns info into their own df to add in again later on
# or else they will disappear when converting to spatial object with st_as_sf
mmwatch_latlon <- mams_new %>% dplyr::select(SPP,
                                             LAT..N.,
                                             LONG..W.,
                                             Date)

# Check the CRS
st_crs(mams_new) # this is non-spatial so the CRS is NA

# Convert data to a spatial object with CRS 4326 (since that is regular lat/lon) - this is still unprojected
mams_new_sp <- st_as_sf(mams_new, coords = c("LONG..W.", "LAT..N."), crs = 4326) 

# Project the data to Alaska Albers 3338
mams_new_sp <- mams_new_sp %>% st_transform(crs = 3338)

# Add back in lat/lon cols using left join to merge lat/lon data onto spatial data rather than spatial data onto lat/lon data
mams_sp <- mams_new_sp %>% left_join(mmwatch_latlon, by = "Date")

# Add a new column to specify data source so you can backtrack later if needed
mams_sp$source <- "Marine Mammal Watch Laurier 2019"
# May also want to add other columns that help add information about the data source (e.g., vessel based, plane based, etc.)

# Remove extraneous columns again
mams_sp <- mams_sp %>% dplyr::select(SPP.x,
                                     NO,
                                     LAT..N.,
                                     LONG..W.,
                                     Date,
                                     source)

# Convert date to a standard format

# Separate Date into year, month, day cols
# Add a year column since all observations were recorded in 2017
# Substring month and day columns
mams_sp$year <- 2019
mams_sp$month <- 7
mams_sp$day <- substr(mams_sp$Date, 3, 4)

# Rename SPP code with common species name
mams_sp$SPP.x[mams_sp$SPP.x == 11] <- "Walrus"
mams_sp$SPP.x[mams_sp$SPP.x == 12] <- "Bearded Seal"
mams_sp$SPP.x[mams_sp$SPP.x == 14] <- "Spotted Seal"

# Add scientific name column
mams_sp$scientificName[mams_sp$SPP.x == "Walrus"] <- "Odobenus rosmarus divergens"
mams_sp$scientificName[mams_sp$SPP.x == "Bearded Seal"] <- "Erignathus barbatus"
mams_sp$scientificName[mams_sp$SPP.x == "Spotted Seal"] <- "Phoca largha"

# Remove extraneous cols, rearrange, and rename
mams_sp2 <- mams_sp %>% dplyr::select(scientificName,
                                      SPP.x,
                                      year,
                                      month,
                                      day,
                                      NO,
                                      LAT..N.,
                                      LONG..W.,
                                      source) %>% 
  rename(commonName = SPP.x,
         groupSize = NO,
         decimalLat = LAT..N.,
         decimalLon = LONG..W.)

## Tada now you have a clean and tidy dataset to work with and merge

View(mams_sp2)

# Can save this data as an output as both shp and csv
# st_write(mams_sp2, "../Data_Processed/6_nsf_dbo_laurier_2019.csv")
