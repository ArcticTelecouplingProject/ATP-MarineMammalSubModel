# 07Nov2022
# Author: Sydney Waloven and Natalia Portales

# Source: NSF ARCSS Google Drive
# Occurrence data of the three species to be evaluated in the project:
# Pacific walrus (Odobenus rosmarus divergens), bearded seal (Erignathus barbatus),
# and spotted seal (Phoca largha).
# These datasets were obtained from the Global Biodiversity Information Facility (GBIF):
# https://www.gbif.org/

# Only species featured in this script is Pacific walrus.

# This dataset was tidied and ready for data integration on 07Nov2022.
# Last edited: 6Apr2023

# Libraries and Packages --------------------------------------------------

library(terra)
library(sf)
library(tidyverse)

# Clear history 
rm(list=ls())

# Pacific walrus data set #1 ----------------------------------------------

# Import raw walrus occurrence data
datadir <- "C:/Users/Natalia Portales/Michigan State University/Waloven, Sydney - ATP-MarineMammal-DataRaw/"
datadir <- "../../ATP-MarineMammal-DataRaw/"
 
walrus_gbif <- st_read(paste0(datadir, "1_arcss_gbif/odobenus_rosmarus_divergens"))

head(walrus_gbif)
colnames(walrus_gbif)

# Create a datafame to generate metadata summaries on the occurrence data at various steps of tidying
OccurrenceSummaries <- data.frame(name = "walrus_gbif") 
OccurrenceSummaries$before <- length(walrus_gbif$specificEp)

# Add to metadata datafame for summaries on the occurrence data at various steps of tidying
OccurrenceSummaries$isolatedSpecies <- length(walrus_gbif$specificEp)

# Transform to Alaska Albers projection
walrus_gbif_3338 <- walrus_gbif %>% st_transform(crs = 3338)


# Check to see if it's properly projected (NAD83 / Alaska Albers)
st_crs(walrus_gbif_3338)


# Clean up this dataset further

## Get rid of unneeded columns 
walrus_gbif_new <- walrus_gbif_3338[ , 12:50]

## Get rid of unnecessary NA values (if any)
walrus_gbif_new <- walrus_gbif_new[!is.na(walrus_gbif_new$decimalLat), ]

# Remove extraneous columns - use select function to choose which columns to keep in new df
colnames(walrus_gbif_new)
wal_gbif_new <- walrus_gbif_new %>% dplyr::select(genericNam,
                                                  specificEp,
                                                  infraspeci,
                                                  decimalLat,
                                                  decimalLon,
                                                  year,
                                                  month,
                                                  day,
                                                  basisOfRec)

# Add to metadata datafame for summaries on the occurrence data at various steps of tidying
OccurrenceSummaries$columnsofInterest <- length(wal_gbif_new$specificEp)

## Geometry column will automatically be included too because of the spatial structure
## Can get rid of with st_drop_geometry()

# Add a new column to specify data source to backtrack later
wal_gbif_new$source <- "ARCSS GBIF Pacific Walrus"

# How is R classifying each column of data?
lapply(wal_gbif_new, class)

# Convert numeric columns to numeric - probably a more succinct way to do this but that's okay
wal_gbif_new$decimalLat <- as.numeric(wal_gbif_new$decimalLat)
wal_gbif_new$decimalLon <- as.numeric(wal_gbif_new$decimalLon)
wal_gbif_new$year <- as.numeric(walrus_gbif_new$year)
wal_gbif_new$month <- as.numeric(walrus_gbif_new$month)
wal_gbif_new$day <- as.numeric(wal_gbif_new$day)

# Combine species and subspecies columns into one col and make one character string
wal_gbif_new$genericNam <- gsub("\"", "", wal_gbif_new$genericNam)
wal_gbif_new$specificEp <- gsub("\"", "", wal_gbif_new$specificEp)
wal_gbif_new$infraspeci <- gsub("\"", "", wal_gbif_new$infraspeci)
# Add that character string to a new column
wal_gbif_new$scientificName <- paste(wal_gbif_new$genericNam, wal_gbif_new$specificEp, wal_gbif_new$infraspeci)

# Move scientificName column to first column
walrus_gbif_clean <- wal_gbif_new %>% dplyr::select(scientificName, everything())

# Remove old columns of genus, species, and subspecies
walrus_gbif_clean <- walrus_gbif_clean %>% dplyr::select(scientificName,
                                                         year,
                                                         month,
                                                         day,
                                                         decimalLat,
                                                         decimalLon,
                                                         basisOfRec,
                                                         source,
                                                         geometry)

# Add a common name column to the walrus_gbif_clean df and fill it with 'Walrus'
walrus_gbif_clean$commonName <- "Walrus"

# Rename basisOfRec column to methodObservation column
walrus_gbif_clean <- walrus_gbif_clean %>% rename("methodObservation" = "basisOfRec")

# Rename values of method column
walrus_gbif_clean$methodObservation[walrus_gbif_clean$methodObservation == '"HUMAN_OBSERVATION"'] <- 'Human observation'
walrus_gbif_clean$methodObservation[walrus_gbif_clean$methodObservation == '"PRESERVED_SPECIMEN"'] <- 'Preserved specimen'

# Missing a groupSize column in the walrus_gbif_merge df??
# Add groupSize column to walrus_gbif_merge df based on whether there are no NAs in the year, month, day cols
## Reference for code:
## https://www.geeksforgeeks.org/how-to-add-a-column-based-on-other-columns-in-r-dataframe/
walrus_gbif_merge <- transform(walrus_gbif_clean, groupSize = ifelse
                               (is.na(year) & is.na(month) & is.na(day), 0, 1))
# Right now this is marking records with no YMD as 0
# Decide whether to mark records with an NA in any part of the YMD as 0?

# Rearrange column names to match dfs - or I think this can be solved with whatever merge function ends up used
walrus_gbif_merge <- walrus_gbif_merge %>% dplyr::select(scientificName,
                                                         commonName,
                                                         year,
                                                         month,
                                                         day,
                                                         groupSize,
                                                         decimalLat,
                                                         decimalLon,
                                                         methodObservation,
                                                         source,
                                                         geometry)

# Convert to SpatialPointsDataFrame ---------------------------------------

# Need to convert to SpatialPointsDataFrame instead of multipoints df first

# Cast geometry to another type with st_cast()
walrus_gbif_sp <- walrus_gbif_merge %>% st_cast("POINT")


# quantifying number of occurrences before eliminating repeats
# nrow(walrus_gbif_sp)

# Eliminate repeat occurrences
# https://statisticsglobe.com/unique-rows-of-data-frame-based-on-selected-columns-in-r
unique_walrus <- distinct(walrus_gbif_sp)

# quantifying number of occurrences after eliminating repeats
# nrow(unique_walrus)

# Remove Nas
unique_walrus_omit <- na.omit(unique_walrus)

# Add to metadata datafame for summaries on the occurrence data at various steps of tidying
OccurrenceSummaries$uniqueOccurrences <- length(unique_walrus_omit$scientificName)

View(OccurrenceSummaries)

## Tada now you have a clean and tidy dataset to work with and merge

View(unique_walrus_omit)

# Save this df as a csv for importing - need to use st_write because of spatial geometry
# st_write(walrus_gbif_sp, "../Data_Processed/1-1_ARCSS_GBIF_walrus/1-1_walrus_gbif_sp.csv")

# Save metadata dataframe
# write.csv(OccurrenceSummaries, "../Data_Processed/Metadata/Metadata_1-1_walrus_gbif.csv")
