# 07Nov2022
# Author: Sydney Waloven

# Source: NSF ARCSS Google Drive
# Occurrence data of the three species to be evaluated in the project:
# Pacific walrus (Odobenus rosmarus divergens), bearded seal (Erignathus barbatus),
# and spotted seal (Phoca largha).
# These datasets were obtained from the Global Biodiversity Information Facility (GBIF):
# https://www.gbif.org/

# Only species featured in this script is bearded seal.

# This dataset was tidied and ready for data integration on 07Nov2022.
# Last edited: 06Jan2022

# Libraries and Packages --------------------------------------------------

library(terra)
library(sf)
library(tidyverse)

# Clear history 
rm(list=ls())


# Bearded seal data set #1 ------------------------------------------------


# Import raw spotted seal occurrence data
bearded_gbif <- st_read("../../ATP-MarineMammal-DataRaw/1_arcss_gbif/erignathus_barbatus")

head(bearded_gbif)

# Transform to Alaska Albers projection
bearded_gbif_3338 <- bearded_gbif %>% st_transform(crs = 3338)

# Check to see if it's properly projected (NAD83 / Alaska Albers)
st_crs(bearded_gbif_3338)


# Clean up this dataset further

## Get rid of unneeded columns 
bearded_gbif_new <- bearded_gbif_3338[ , 14:50]

## Get rid of unnecessary NA values (if any)
bearded_gbif_new <- bearded_gbif_new[!is.na(bearded_gbif_new$decimalLat), ]

# Remove extraneous columns - use select function to choose which columns to keep in new df
colnames(bearded_gbif_new)
beard_gbif <- bearded_gbif_new %>% dplyr::select(genericNam,
                                                    specificEp,
                                                    decimalLat,
                                                    decimalLon,
                                                    year,
                                                    month,
                                                    day)
## Geometry column will automatically be included too because of the spatial structure
## Can get rid of with st_drop_geometry()

# Add a new column to specify data source to backtrack later
beard_gbif$source <- "ARCSS GBIF Bearded Seal"

# How is R classifying each column of data?
lapply(beard_gbif, class)

# Convert numeric columns to numeric - probably a more succinct way to do this but that's okay
beard_gbif$decimalLat <- as.numeric(beard_gbif$decimalLat)
beard_gbif$decimalLon <- as.numeric(beard_gbif$decimalLon)
beard_gbif$year <- as.numeric(beard_gbif$year)
beard_gbif$month <- as.numeric(beard_gbif$month)
beard_gbif$day <- as.numeric(beard_gbif$day)

# Combine species and subspecies columns into one col and make one character string
beard_gbif$genericNam <- gsub("\"", "", beard_gbif$genericNam)
beard_gbif$specificEp <- gsub("\"", "", beard_gbif$specificEp)
# Add that character string to a new column
beard_gbif$scientificName <- paste(beard_gbif$genericNam, beard_gbif$specificEp)

# Move scientificName column to first column
beard_gbif_clean <- beard_gbif %>% select(scientificName, everything())

# Remove old columns of genus and species
beard_gbif_clean <- beard_gbif_clean %>% dplyr::select(scientificName,
                                                           year,
                                                           month,
                                                           day,
                                                           decimalLat,
                                                           decimalLon,
                                                           source,
                                                           geometry)

# Add a common name column to the walrus_gbif_clean df and fill it with 'Bearded Seal'
beard_gbif_clean$commonName <- "Bearded Seal"

# Missing a groupSize column in the beard_gbif_clean df??
# Add groupSize column to beard_gbif_clean df based on whether there are no NAs in the year, month, day cols
## Reference for code:
## https://www.geeksforgeeks.org/how-to-add-a-column-based-on-other-columns-in-r-dataframe/
beard_gbif_merge <- transform(beard_gbif_clean, groupSize = ifelse
                                (is.na(year) & is.na(month) & is.na(day), 0, 1))
# Right now this is marking records with no YMD as 0
# Decide whether to mark records with an NA in any part of the YMD as 0?

# Rearrange column names to match dfs - or I think this can be solved with whatever merge function ends up used
beard_gbif_merge <- beard_gbif_merge %>% dplyr::select(scientificName,
                                                           commonName,
                                                           year,
                                                           month,
                                                           day,
                                                           groupSize,
                                                           decimalLat,
                                                           decimalLon,
                                                           source,
                                                           geometry)

# Convert to SpatialPointsDataFrame ---------------------------------------

# Need to convert to SpatialPointsDataFrame instead of multipoints df first

# Cast geometry to another type with st_cast()
bearded_gbif_sp <- beard_gbif_merge %>% st_cast("POINT")


## Tada now you have a clean and tidy dataset to work with and merge

View(bearded_gbif_sp)


# Save this df as a csv for importing
st_write(bearded_gbif_sp, "../Data_Processed/1-3_bearded_arcss_sp.csv")

