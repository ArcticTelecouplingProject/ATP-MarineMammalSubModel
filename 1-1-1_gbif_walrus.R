# 07Jun2023
# Author: Sydney Waloven

# Source: GBIF
# Occurrence data of the three species to be evaluated in the project:
# This dataset was obtained from a query on Global Biodiversity Information Facilit
# (GBIF): https://www.gbif.org/

# Only species featured in this script is Pacific walrus.

# This dataset was tidied and ready for data integration on .
# Last edited: 07Jun2023

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
walrus_gbif <- st_read(paste0(datadir, "additional_data/new_gbif_queries/gbif_odobenus_20230522.csv"))

# Look at column names
colnames(walrus_gbif)

# Get rid of unnecessary NA values (if any) in the latitude column
walrus_gbif <- walrus_gbif[!is.na(walrus_gbif$decimalLatitude), ]

# Remove extraneous columns - use select function to choose which columns to keep in new df
walrus_new <- walrus_gbif %>% dplyr::select(verbatimScientificName,
                                            decimalLatitude,
                                            decimalLongitude,
                                            year,
                                            month,
                                            day,
                                            basisOfRecord)

# Set projection using lat/lon cols
walrus_new <- st_as_sf(walrus_new, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326, remove=FALSE)
                                           
# Transform to Alaska Albers projection
walrus_3338 <- walrus_new %>% st_transform(crs = 3338)

# Check to see if it's properly projected (NAD83 / Alaska Albers)
st_crs(walrus_3338)

## Geometry column will automatically be included too because of the spatial structure
## Can get rid of with st_drop_geometry()

# Add a new column to specify data source to backtrack later
walrus_3338$source <- "GBIF Query"

# How is R classifying each column of data?
lapply(walrus_3338, class)

# Convert numeric columns to numeric - probably a more succinct way to do this but that's okay
walrus_3338$decimalLatitude <- as.numeric(walrus_3338$decimalLatitude)
walrus_3338$decimalLongitude <- as.numeric(walrus_3338$decimalLongitude)
walrus_3338$year <- as.numeric(walrus_gbif$year)
walrus_3338$month <- as.numeric(walrus_gbif$month)
walrus_3338$day <- as.numeric(walrus_3338$day)

## Pick up from here

# Move scientificName column to first column
walrus_gbif_clean <- walrus_3338 %>% dplyr::select(verbatimScientificName, everything())

# Remove old columns of genus, species, and subspecies
walrus_gbif_clean <- walrus_gbif_clean %>% dplyr::select(scientificName,
                                                         year,
                                                         month,
                                                         day,
                                                         decimalLatitude,
                                                         decimalLongitude,
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
                                                         decimalLatitude,
                                                         decimalLongitude,
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

# Add to metadata dataframe for summaries on the occurrence data at various steps of tidying
OccurrenceSummaries$uniqueOccurrences <- length(unique_walrus_omit$scientificName)

View(OccurrenceSummaries)

## Tada now you have a clean and tidy dataset to work with and merge

View(unique_walrus_omit)

# Save this df as a csv for importing - need to use st_write because of spatial geometry
# st_write(unique_walrus_omit, "../Data_Processed/1-1_ARCSS_GBIF_walrus/1-1_walrus_gbif_sp.shp")

# Save metadata dataframe
# write.csv(OccurrenceSummaries, "../Data_Processed/Metadata/Metadata_1-1_walrus_gbif.csv")



# Graveyard ---------------------------------------------------------------

# # Create a dataframe to generate metadata summaries on the occurrence data at various steps of tidying
# OccurrenceSummaries <- data.frame(name = "walrus_gbif") 
# OccurrenceSummaries$before <- length(walrus_gbif$specificEp)
# 
# # Add to metadata dataframe for summaries on the occurrence data at various steps of tidying
# OccurrenceSummaries$isolatedSpecies <- length(walrus_gbif$specificEp)
#
# # Add to metadata dataframe for summaries on the occurrence data at various steps of tidying
# OccurrenceSummaries$columnsofInterest <- length(walrus_3338$specificEp)
