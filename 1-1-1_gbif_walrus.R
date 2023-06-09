# 07Jun2023
# Author: Sydney Waloven

# Source: GBIF
# Occurrence data of the three species to be evaluated in the project:
# This dataset was obtained from a query on Global Biodiversity Information Facilit
# (GBIF): https://www.gbif.org/

# Only species featured in this script is Pacific walrus.

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
walrus_gbif <- st_read(paste0(datadir, "1-1_gbif_queries/gbif_odobenus_20230522.csv"))

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

# Set CRS -----------------------------------------------------------------

# Set projection using lat/lon cols
walrus_new <- st_as_sf(walrus_new, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326, remove=FALSE)
                                           
# Transform to Alaska Albers projection
walrus_3338 <- walrus_new %>% st_transform(crs = 3338)

# Check to see if it's properly projected (NAD83 / Alaska Albers)
st_crs(walrus_3338)

## Geometry column will automatically be included too because of the spatial structure
## Can get rid of with st_drop_geometry()

# How is R classifying each column of data?
lapply(walrus_3338, class)

# Convert numeric columns to numeric - probably a more succinct way to do this but that's okay
walrus_3338$decimalLatitude <- as.numeric(walrus_3338$decimalLatitude)
walrus_3338$decimalLongitude <- as.numeric(walrus_3338$decimalLongitude)
walrus_3338$year <- as.numeric(walrus_gbif$year)
walrus_3338$month <- as.numeric(walrus_gbif$month)
walrus_3338$day <- as.numeric(walrus_3338$day)


# Modify columns ----------------------------------------------------------

# Add a new column to specify data source to backtrack later
walrus_3338$source <- "GBIF Query"

# Add a common name column and fill it with 'Walrus'
walrus_3338$commonName <- "Walrus"

# Rename basisOfRecord column to methodObservation column
walrus_3338 <- walrus_3338 %>% rename("methodObservation" = "basisOfRecord")

# Check to see what unique values there are in the methodObservation column
unique(walrus_3338$methodObservation)
# Rename values of method column for consistency with other datasets
walrus_3338$methodObservation[walrus_3338$methodObservation == "HUMAN_OBSERVATION"] <- 'Human observation'

# Add groupSize column based on whether there are no NAs in the year, month, day cols
## Reference for code:
## https://www.geeksforgeeks.org/how-to-add-a-column-based-on-other-columns-in-r-dataframe/
walrus_3338 <- transform(walrus_3338, groupSize = ifelse
                               (is.na(year) & is.na(month) & is.na(day), 0, 1))
# This checks whether all three columns (year, month, and day) contain missing values (NA)
# using the is.na() function. If all three columns have missing values, the condition
# evaluates to TRUE and fills with 0, otherwise, it evaluates to FALSE and fills with 1.
# Decide whether to mark records with an NA in any part of the YMD as 0?

# Rearrange column names to match other dfs
walrus_merge <- walrus_3338 %>% dplyr::select(verbatimScientificName,
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
# nrow(walrus_merge)

# Eliminate repeat occurrences
# https://statisticsglobe.com/unique-rows-of-data-frame-based-on-selected-columns-in-r
unique_walrus <- distinct(walrus_merge)

# Quantify number of occurrences after eliminating repeats
# nrow(unique_walrus)

# Remove NAs
unique_walrus_omit <- na.omit(unique_walrus)

## Tada now you have a clean and tidy dataset to work with and merge

View(unique_walrus_omit)

# Save this df as a csv for importing - need to use st_write because of spatial geometry
# st_write(unique_walrus_omit, "../Data_Processed/1-1-1_gbif_walrus/1-1-1_gbif_walrus.csv")


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
#
# Add to metadata dataframe for summaries on the occurrence data at various steps of tidying
# OccurrenceSummaries$uniqueOccurrences <- length(unique_walrus_omit$scientificName)
#
# View(OccurrenceSummaries)
#
# Save metadata dataframe
# write.csv(OccurrenceSummaries, "../Data_Processed/Metadata/Metadata_1-1_walrus_gbif.csv")