# 08Jan2023
# Author: Sydney Waloven and Natalia Portales

# Source: Arctic Data Center
# This dataset was obtained from Marine Mammal Sightings During Sir Wilfrid Laurier
# Cruise in July 2019
# https://arcticdata.io/catalog/view/doi%3A10.18739%2FA2DZ0320P

# Species isolated in this dataset: walrus, bearded seal, spotted seal

# This dataset was tidied and ready for data integration on 08Jan2023
# Last edited: 6Apr2023



# Libraries and Packages --------------------------------------------------

library(terra)
library(sf)
library(tidyverse)


# Clear history 
rm(list=ls())


# DBO Laurier 2019 --------------------------------------------------------


# Read in the data
datadir <- "C:/Users/Natalia Portales/Michigan State University/Waloven, Sydney - ATP-MarineMammal-DataRaw/"
datadir <- "../../ATP-MarineMammal-DataRaw/"

dbo_swl_2019 <- read.csv(paste0(datadir,"6_nsf_dbo_laurier_2019/data/DBO_SWL_2019.csv"))
head(dbo_swl_2019)

# Create a dataframe to generate metadata summaries on the occurrence data at various steps of tidying
OccurrenceSummaries <- data.frame(name = "nsf_dbo_swl_2019") 
OccurrenceSummaries$before <- length(dbo_swl_2019$SPP)

# Isolate out species of interest (see metadata for more details)
# SPP corresponds to species
# 11 = walrus
# 12 = bearded seal
# 14 = spotted seal
mmwatch_mams <- dbo_swl_2019[dbo_swl_2019$SPP %in% c(11, 12, 14), ]

# Add to metadata dataframe for summaries on the occurrence data at various steps of tidying
OccurrenceSummaries$isolatedSpecies <- length(mmwatch_mams$SPP[which(mmwatch_mams$SPP == "11")])

# Remove extraneous columns of info 
colnames(mmwatch_mams)
mams_new <- mmwatch_mams %>% dplyr::select(SPP,
                                           Date,
                                           LAT..N.,
                                           LONG..W.,
                                           NO)

# Add to metadata dataframe for summaries on the occurrence data at various steps of tidying
OccurrenceSummaries$columnsofInterest <- length(mams_new$SPP[which(mams_new$SPP == "11")])


# Convert to spatial df ---------------------------------------------------


# Save lat/lon columns info into their own df to add in again later on
# or else they will disappear when converting to spatial object with st_as_sf
# mmwatch_latlon <- mams_new %>% dplyr::select(SPP,
                                            # LAT..N.,
                                            # LONG..W.,
                                            # Date)

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

