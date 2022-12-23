# 07Nov2022
# Author: Sydney Waloven

# Source: NSF ARCSS Google Drive
# Occurrence data of the three species to be evaluated in the project:
# Pacific walrus (Odobenus rosmarus divergens), bearded seal (Erignathus barbatus),
# and spotted seal (Phoca largha).
# These datasets were obtained from the Global Biodiversity Information Facility (GBIF):
# https://www.gbif.org/

# Only species featured in this script is the spotted seal.

# This dataset was tidied and ready for data integration on 07Nov2022.
# Last edited: 23Dec2022.

# Libraries and Packages --------------------------------------------------

library(terra)
library(sf)
library(tidyverse)

# Clear history 
rm(list=ls())

# Spotted seal data set #1 ------------------------------------------------


# Import raw spotted seal occurrence data
spotted_gbif <- st_read("../../ATP-MarineMammal-DataRaw/phoca_largha")

head(spotted_gbif)

# Transform to Alaska Albers projection
spotted_gbif_3338 <- spotted_gbif %>% st_transform(crs = 3338)

# Check to see if it's properly projected (NAD83 / Alaska Albers)
st_crs(spotted_gbif_3338)


# Clean up this dataset further

## Get rid of unneeded columns 
spotted_gbif_new <- spotted_gbif_3338[ , 14:50]

## Get rid of unnecessary NA values (if any)
spotted_gbif_new <- spotted_gbif_new[!is.na(spotted_gbif_new$decimalLat), ]

# Remove extraneous columns - use select function to choose which columns to keep in new df
colnames(spotted_gbif_new)
spot_gbif_new <- spotted_gbif_new %>% dplyr::select(genericNam,
                                                  specificEp,
                                                  decimalLat,
                                                  decimalLon,
                                                  year,
                                                  month,
                                                  day)
## Geometry column will automatically be included too because of the spatial structure
## Can get rid of with st_drop_geometry()

# Add a new column to specify data source to backtrack later
spot_gbif_new$source <- "ARCSS GBIF Spotted Seal"

# How is R classifying each column of data?
lapply(spot_gbif_new, class)

# Convert numeric columns to numeric - probably a more succinct way to do this but that's okay
spot_gbif_new$decimalLat <- as.numeric(spot_gbif_new$decimalLat)
spot_gbif_new$decimalLon <- as.numeric(spot_gbif_new$decimalLon)
spot_gbif_new$year <- as.numeric(spotted_gbif_new$year)
spot_gbif_new$month <- as.numeric(spotted_gbif_new$month)
spot_gbif_new$day <- as.numeric(spot_gbif_new$day)

# Combine species and subspecies columns into one col and make one character string
spot_gbif_new$genericNam <- gsub("\"", "", spot_gbif_new$genericNam)
spot_gbif_new$specificEp <- gsub("\"", "", spot_gbif_new$specificEp)
# Add that character string to a new column
spot_gbif_new$scientificName <- paste(spot_gbif_new$genericNam, spot_gbif_new$specificEp)

# Move scientificName column to first column
spotted_gbif_clean <- spot_gbif_new %>% select(scientificName, everything())

# Remove old columns of genus and species
spotted_gbif_clean <- spotted_gbif_clean %>% dplyr::select(scientificName,
                                                         year,
                                                         month,
                                                         day,
                                                         decimalLat,
                                                         decimalLon,
                                                         source,
                                                         geometry)

# Add a common name column to the spotted_gbif_clean df and fill it with 'Walrus'
spotted_gbif_clean$commonName <- "Spotted seal"

# Missing a groupSize column in the spotted_gbif_clean df??
# Add groupSize column to spotted_gbif_clean df based on if there are NO NAs in the year, month, day cols
# If there is a YMD, then this will add a 1 to the groupSize column
## Reference for code:
## https://www.geeksforgeeks.org/how-to-add-a-column-based-on-other-columns-in-r-dataframe/
spotted_gbif_merge <- transform(spotted_gbif_clean, groupSize = ifelse
                               (is.na(year) & is.na(month) & is.na(day), 0, 1))
# Right now this is marking records with no YMD as 0 - Decide whether to mark records with an NA in any part of the YMD as 0?

# Rearrange column names to match dfs - or I think this can be solved with whatever merge function ends up used
spotted_gbif_merge <- spotted_gbif_merge %>% dplyr::select(scientificName,
                                                         commonName,
                                                         year,
                                                         month,
                                                         day,
                                                         groupSize,
                                                         decimalLat,
                                                         decimalLon,
                                                         source,
                                                         geometry)

## Tada now you have a clean and tidy dataset to work with and merge

View(spotted_gbif_merge)

# Save this df as a csv for importing
# st_write(spotted_gbif_merge, "../ATP_Data_Integration/Data_Processed/1-2_spotted_arcss_gbif.csv")
