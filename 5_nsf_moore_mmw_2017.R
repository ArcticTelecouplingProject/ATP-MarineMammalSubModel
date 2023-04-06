# 08Jan2023
# Author: Sydney Waloven and Natalia Portales

# Source: Arctic Data Center
# This dataset was obtained from: Arctic Data Center Marine Mammal Watch,
# Northern Bering Sea and Chukchi Sea August 2017
# https://arcticdata.io/catalog/view/doi%3A10.18739%2FA27P8TD2J

# Species isolated in this dataset: walrus, bearded seal, spotted seal

# This dataset was tidied and ready for data integration on 08Jan2023
# Last edited: 6Apr2023



# Libraries and Packages --------------------------------------------------

library(terra)
library(sf)
library(tidyverse)


# Clear history 
rm(list=ls())


# Marine Mammal Watch 2017 ------------------------------------------------

# Read in the data
datadir <- "C:/Users/Natalia Portales/Michigan State University/Waloven, Sydney - ATP-MarineMammal-DataRaw/"
datadir <- "../../ATP-MarineMammal-DataRaw/"

mmwatch2017 <- read.csv(paste0(datadir,"5_nsf_moore_mmw_2017/data/DBO_MMWatch_HLY1702_August2017_SMoore.csv"))
head(mmwatch2017)

# Create a datafame to generate metadata summaries on the occurrence data at various steps of tidying
OccurrenceSummaries <- data.frame(name = "nsf_mmwatch2017") 
OccurrenceSummaries$before <- length(mmwatch2017$SPP)

# Isolate out species of interest (see metadata for more details)
# SPP corresponds to species
# 11 = walrus
# 12 = bearded seal
# 14 = spotted seal
mmwatch_mams <- mmwatch2017[mmwatch2017$SPP %in% c(11, 12, 14), ]

# Add to metadata datafame for summaries on the occurrence data at various steps of tidying
OccurrenceSummaries$isolatedSpecies <- length(mmwatch_mams$SPP[which(mmwatch_mams$SPP == "11")])

# Remove extraneous columns of info 
colnames(mmwatch_mams)
mams_new <- mmwatch_mams %>% dplyr::select(SPP,
                                           Date,
                                           LAT..N.,
                                           LONG..W.,
                                           No.)

# Add to metadata datafame for summaries on the occurrence data at various steps of tidying
OccurrenceSummaries$columnsofInterest <- length(mams_new$SPP[which(mams_new$SPP == "11")])


# Convert to spatial df ---------------------------------------------------

# Change format of lat/lon columns from degrees-decimal minutes to decimal degrees
## substr() takes the column name, starting position, and length of the string as arguments, returning
## the substring of the specific column - use this to separate LAT..N. and LONG..W. into two columns
mams_new$deg_lat <- as.numeric(substr(mams_new$LAT..N., 1, 2))
mams_new$degfrac_lat <- as.numeric(substr(mams_new$LAT..N., 3, 6))/60

mams_new$deg_lon <- as.numeric(substr(mams_new$LONG..W., 1, 3))
mams_new$degfrac_lon <- as.numeric(substr(mams_new$LONG..W., 4, 6))/60

# [[:punct:]] removes punctuation like the decimal point

# Combine deg and min cols to make decimal degrees col that character string to a new colum
mams_new$decimalLat <- mams_new$deg_lat + mams_new$degfrac_lat
mams_new$decimalLon <- mams_new$deg_lon + mams_new$degfrac_lon

# Replace space between deg and min with a decimal point
# mams_new$decimalLat <- gsub("[[:blank:]]", ".", mams_new$decimalLat)
# mams_new$decimalLon <- gsub("[[:blank:]]", ".", mams_new$decimalLon)

# Figure out how R is classifying each column of data
lapply(mams_new, class)

# Convert numeric columns to numeric if not already
mams_new$decimalLat <- as.numeric(mams_new$decimalLat)
mams_new$decimalLon <- as.numeric(mams_new$decimalLon)
mams_new$No. <- as.integer(mams_new$No.)

# Make longitude column negative to reflect proper hemisphere
mams_new$decimalLon <- mams_new$decimalLon*(-1)

# Save lat/lon columns info into their own df to add in again later on
# or else they will disappear when converting to spatial object with st_as_sf
# mmwatch_latlon <- mams_new %>% dplyr::select(SPP,
                                            # decimalLat,
                                             # decimalLon,
                                             # Date)

# Check the CRS
st_crs(mams_new) # this is non-spatial so the CRS is NA

# Convert data to a spatial object with CRS 4326 (since that is regular lat/lon) - this is still unprojected
mams_new_sp <- st_as_sf(mams_new, coords = c("decimalLon", "decimalLat"), crs = 4326, remove=FALSE) 

# Project the data to Alaska Albers 3338
mams_sp <- mams_new_sp %>% st_transform(crs = 3338)

# Add back in lat/lon cols using left join to merge lat/lon data onto spatial data rather than spatial data onto lat/lon data
# mams_sp <- mams_new_sp %>% left_join(mmwatch_latlon, by = "Date")

# Add a new column to specify data source so you can backtrack later if needed
mams_sp$source <- "Marine Mammal Watch Aug-Sep 2017"
# May also want to add other columns that help add information about the data source (e.g., vessel based, plane based, etc.)
# Add a new column to specify the method of data collection used
mams_sp$methodObservation <- "Research cruise visual observation survey"

# Remove extraneous columns again
mams_sp <- mams_sp %>% dplyr::select(SPP,
                                     No.,
                                     decimalLat,
                                     decimalLon,
                                     Date,
                                     methodObservation,
                                     source)

# Convert date to a standard format

# Separate Date into year, month, day cols
# Add a year column since all observations were recorded in 2017
# Substring month and day columns
mams_sp$year <- 2017
mams_sp$month <- substr(mams_sp$Date, 1, 1)
mams_sp$day <- substr(mams_sp$Date, 3, 4)
mams_sp$day <- gsub("/", "", as.character(mams_sp$day))

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
                                      No.,
                                      decimalLat,
                                      decimalLon,
                                      methodObservation,
                                      source) %>% 
  rename(commonName = SPP,
         groupSize = No.)

# quantifying number of occurrences before eliminating repeats
# nrow(mams_sp2)

# Eliminate repeat occurrences
# https://statisticsglobe.com/unique-rows-of-data-frame-based-on-selected-columns-in-r
unique_mams <- distinct(mams_sp2)

# quantifying number of occurrences after eliminating repeats
# nrow(unique_mams)

# Remove Nas
unique_mams_omit <- na.omit(unique_mams)

# Add to metadata datafame for summaries on the occurrence data at various steps of tidying
OccurrenceSummaries$uniqueOccurrences <- length(unique_mams_omit$commonName[which(unique_mams_omit$commonName == "Walrus")])

View(OccurrenceSummaries)

## Tada now you have a clean and tidy dataset to work with and merge

View(unique_mams_omit)

# Can save this data as an output as both shp and csv
# st_write(unique_mams_omit, "../Data_Processed/5_nsf_moore_mmw_2017/5_nsf_moore_mmw_2017.shp")

# Save metadata dataframe
# write.csv(OccurrenceSummaries, "../Data_Processed/Metadata/Metadata_5_nsf_moore_mmw_2017.csv")

