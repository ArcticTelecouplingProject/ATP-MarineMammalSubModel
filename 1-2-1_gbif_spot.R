# 08Jun2023
# Author: Sydney Waloven

# Source: GBIF
# Occurrence data of the three species to be evaluated in the project:
# This dataset was obtained from a query on Global Biodiversity Information Facilit
# (GBIF): https://www.gbif.org/

# Only species featured in this script is spotted seal.

# This dataset was tidied and ready for data integration on 08Jun2023.
# Last edited: 08Jun2023

# Libraries and Packages --------------------------------------------------

library(terra)
library(sf)
library(tidyverse)

# Clear history 
rm(list=ls())


# Importing data ----------------------------------------------------------

# Assign directory
datadir <- "../../ATP-MarineMammal-DataRaw/"

# Import raw walrus occurrence data from gbif query
largha_gbif <- st_read(paste0(datadir, "1-1_gbif_queries/gbif_largha_20230522.csv"))

# Look at column names
colnames(largha_gbif)

# Get rid of unnecessary NA values (if any) in the latitude column
largha_gbif <- largha_gbif[!is.na(largha_gbif$decimalLatitude), ]

# Remove extraneous columns - use select function to choose which columns to keep in new df
largha_new <- largha_gbif %>% dplyr::select(verbatimScientificName,
                                            decimalLatitude,
                                            decimalLongitude,
                                            year,
                                            month,
                                            day,
                                            basisOfRecord)


# Set CRS -----------------------------------------------------------------

# Set projection using lat/lon cols
largha_new <- st_as_sf(largha_new, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326, remove=FALSE)

# Transform to Alaska Albers projection
largha_3338 <- largha_new %>% st_transform(crs = 3338)

# Check to see if it's properly projected (NAD83 / Alaska Albers)
st_crs(largha_3338)

## Geometry column will automatically be included too because of the spatial structure
## Can get rid of with st_drop_geometry()

# How is R classifying each column of data?
lapply(largha_3338, class)

# Convert numeric columns to numeric - probably a more succinct way to do this but that's okay
largha_3338$decimalLatitude <- as.numeric(largha_3338$decimalLatitude)
largha_3338$decimalLongitude <- as.numeric(largha_3338$decimalLongitude)
largha_3338$year <- as.numeric(largha_gbif$year)
largha_3338$month <- as.numeric(largha_gbif$month)
largha_3338$day <- as.numeric(largha_3338$day)

# Modify columns ----------------------------------------------------------

# Add a new column to specify data source to backtrack later
largha_3338$source <- "GBIF Query"

# Add a common name column and fill it with 'Spotted Seal'
largha_3338$commonName <- "Spotted Seal"

# Rename basisOfRecord column to methodObservation column
largha_3338 <- largha_3338 %>% rename("methodObservation" = "basisOfRecord")

# Check to see what unique values there are in the methodObservation column
unique(largha_3338$methodObservation)
# Rename values of method column for consistency with other datasets
largha_3338$methodObservation[largha_3338$methodObservation == 'HUMAN_OBSERVATION'] <- 'Human observation'
largha_3338$methodObservation[largha_3338$methodObservation == 'MACHINE_OBSERVATION'] <- 'Machine observation'

# Add groupSize column based on whether there are no NAs in the year, month, day cols
## Reference for code:
## https://www.geeksforgeeks.org/how-to-add-a-column-based-on-other-columns-in-r-dataframe/
largha_3338 <- transform(largha_3338, groupSize = ifelse
                         (is.na(year) & is.na(month) & is.na(day), 0, 1))
# This checks whether all three columns (year, month, and day) contain missing values (NA)
# using the is.na() function. If all three columns have missing values, the condition
# evaluates to TRUE and fills with 0, otherwise, it evaluates to FALSE and fills with 1.
# Decide whether to mark records with an NA in any part of the YMD as 0?

# Rearrange column names to match other dfs
largha_merge <- largha_3338 %>% dplyr::select(verbatimScientificName,
                                              commonName,
                                              year,
                                              month,
                                              day,
                                              groupSize,
                                              decimalLatitude,
                                              decimalLongitude,
                                              methodObservation,
                                              source,
                                              geometry) %>% rename(
                                                scientificName = verbatimScientificName,
                                                decimalLat = decimalLatitude,
                                                decimalLon = decimalLongitude,
                                              )


# Eliminate repeats -------------------------------------------------------

# Quantify number of occurrences before eliminating repeats
# nrow(largha_merge)

# Eliminate repeat occurrences
# https://statisticsglobe.com/unique-rows-of-data-frame-based-on-selected-columns-in-r
unique_largha <- distinct(largha_merge)

# Quantify number of occurrences after eliminating repeats
# nrow(unique_largha)

# Remove NAs
unique_largha_omit <- na.omit(unique_largha)

## Tada now you have a clean and tidy dataset to work with and merge

View(unique_largha_omit)

# Save this df as a csv for importing - need to use st_write because of spatial geometry
st_write(unique_largha_omit, "../Data_Processed/1-2-1_gbif_spot/1-2-1_gbif_spot.shp")
