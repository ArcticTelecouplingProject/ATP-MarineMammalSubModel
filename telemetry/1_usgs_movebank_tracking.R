# 12Jun2023
# Author: Sydney Waloven

# Source: Movebank
# This dataset was obtained from a study on Movebank titled: 
# Tracking data from the Pacific walrus (Jay et al. 2022)
# Pacific Walrus USGS ASC argosGPS
# https://www.movebank.org/cms/webapp?gwt_fragment=page=studies,path=study1420256397

# Species isolated in this dataset: walrus

# This dataset was tidied and ready for data integration on 
# Last edited: 12Jun2023



# Libraries and Packages --------------------------------------------------

library(terra)
library(sf)
library(tidyverse)


# Clear history 
rm(list=ls())

# Importing data ----------------------------------------------------------

# Read in the data
datadir <- "../../ATP-MarineMammal-DataRaw/"

movebank_argos <- read.csv(paste0(datadir, "telemetry/movebank_walrus_usgs_argos/pacificWalrus_USGS_ASC_argosGPS.csv"))
head(movebank_argos)

movebank_ref <- read.csv(paste0(datadir, "telemetry/movebank_walrus_usgs_argos/pacificWalrus_USGS_ASC_argosGPS-reference-data.csv"))

# Remove extraneous columns of info 
colnames(movebank_argos)
colnames(movebank_ref)
mams_new <- movebank_argos %>% dplyr::select(SPP,
                                           Date,
                                           LAT..N.,
                                           LONG..W.,
                                           NO)

# Convert to spatial df ---------------------------------------------------

# Save lat/lon columns info into their own df to add in again later on
# or else they will disappear when converting to spatial object with st_as_sf

# Check the CRS
st_crs(mams_new) # this is non-spatial so the CRS is NA

# Convert data to a spatial object with CRS 4326 (since that is regular lat/lon) - this is still unprojected
mams_new_sp <- st_as_sf(mams_new, coords = c("LONG..W.", "LAT..N."), crs = 4326, remove=FALSE) 

# Project the data to Alaska Albers 3338
mams_sp <- mams_new_sp %>% st_transform(crs = 3338)

# Add back in lat/lon cols using left join to merge lat/lon data onto spatial data rather than spatial data onto lat/lon data
# mams_sp <- mams_new_sp %>% left_join(mmwatch_latlon, by = "Date")

# Add a new column to specify data source so you can backtrack later if needed
mams_sp$source <- "Marine Mammal Watch Laurier 2019"
# May also want to add other columns that help add information about the data source (e.g., vessel based, plane based, etc.)
# Add a new column to specify the method of data collection used
mams_sp$methodObservation <- "Research cruise visual observation survey"

# Remove extraneous columns again
mams_sp <- mams_sp %>% dplyr::select(SPP,
                                     NO,
                                     LAT..N.,
                                     LONG..W.,
                                     Date,
                                     methodObservation,
                                     source)

# Convert date to a standard format

# Separate Date into year, month, day cols
# Add a year column since all observations were recorded in 2017
# Substring month and day columns
mams_sp$year <- 2019
mams_sp$month <- 7
mams_sp$day <- substr(mams_sp$Date, 3, 4)

# Rename SPP code with common species name
mams_sp$SPP[mams_sp$SPP == 11] <- "Walrus"
mams_sp$SPP[mams_sp$SPP == 12] <- "Bearded Seal"
mams_sp$SPP[mams_sp$SPP == 14] <- "Spotted Seal"

# Add scientific name column
mams_sp$scientificName[mams_sp$SPP == "Walrus"] <- "Odobenus rosmarus divergens"
mams_sp$scientificName[mams_sp$SPP == "Bearded Seal"] <- "Erignathus barbatus"
mams_sp$scientificName[mams_sp$SPP == "Spotted Seal"] <- "Phoca largha"

# Remove extraneous cols, rearrange, and rename
mams_sp2 <- mams_sp %>% dplyr::select(scientificName,
                                      SPP,
                                      year,
                                      month,
                                      day,
                                      NO,
                                      LAT..N.,
                                      LONG..W.,
                                      methodObservation,
                                      source) %>% 
  rename(commonName = SPP,
         groupSize = NO,
         decimalLat = LAT..N.,
         decimalLon = LONG..W.)

# quantifying number of occurrences before eliminating repeats
# nrow(mams_sp2)

# Eliminate repeat occurrences
# https://statisticsglobe.com/unique-rows-of-data-frame-based-on-selected-columns-in-r
unique_mams <- distinct(mams_sp2)

# quantifying number of occurrences after eliminating repeats
# nrow(unique_mams)

# Remove Nas
unique_mams_omit <- na.omit(unique_mams)

# Add to metadata dataframe for summaries on the occurrence data at various steps of tidying
OccurrenceSummaries$uniqueOccurrences <- length(unique_mams_omit$commonName[which(unique_mams_omit$commonName == "Walrus")])

View(OccurrenceSummaries)

## Tada now you have a clean and tidy dataset to work with and merge

View(unique_mams_omit)

# Can save this data as an output as both shp and csv
# st_write(unique_mams_omit, "../Data_Processed/6_nsf_dbo_laurier_2019/6_nsf_dbo_laurier_2019.csv")

# Save metadata dataframe
# write.csv(OccurrenceSummaries, "../Data_Processed/Metadata/Metadata_6_nsf_dbo_laurier_2019.csv")




# Graveyard ---------------------------------------------------------------

# # Create a dataframe to generate metadata summaries on the occurrence data at various steps of tidying
# OccurrenceSummaries <- data.frame(name = "movebank_argos") 
# OccurrenceSummaries$before <- length(movebank_argos$SPP)
# 
# movebank_argos <- movebank_argos[movebank_argos$SPP %in% c(11, 12, 14), ]
#
# # Add to metadata dataframe for summaries on the occurrence data at various steps of tidying
# OccurrenceSummaries$isolatedSpecies <- length(movebank_argos$SPP[which(movebank_argos$SPP == "11")])
# 
# # Add to metadata dataframe for summaries on the occurrence data at various steps of tidying
# OccurrenceSummaries$columnsofInterest <- length(mams_new$SPP[which(mams_new$SPP == "11")])