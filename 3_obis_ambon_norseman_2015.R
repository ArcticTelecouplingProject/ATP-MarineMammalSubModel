# 05Jan2022
# Author: Sydney Waloven

# Source: OBIS
# This dataset was obtained from: AMBON Chukchi Sea research cruse on the Norseman II
# https://www.sciencebase.gov/catalog/item/5e0e24b3e4b0b207aa13782b
# Corresponds with Data_Raw folder: obis_ambon_norseman_2015

# Species isolated in this data set: walrus, bearded seal, spotted seal

# This dataset was tidied and ready for data integration on 05Jan2022
# Last edited: 05Jan2022


# Libraries and Packages --------------------------------------------------

library(terra)
library(sf)
library(tidyverse)

# Clear history 
rm(list=ls())


# AMBON Norseman 2015 -----------------------------------------------------


# Read in the data
birdocc <- read.csv("../../ATP-MarineMammal-DataRaw/3_obis_ambon_norseman_2015/dlog_2015_dwc_occurrence.csv")

head(birdocc)
colnames(birdocc)

# There aren't any locations in this file so check some of the other files that came with it 
birdevent <- read.csv("../../ATP-MarineMammal-DataRaw/3_obis_ambon_norseman_2015/dlog_2015_dwc_event.csv")

head(birdevent) # this one has lat and long - connected to birdocc via eventID column

# Merge via the shared "eventID" column
bird <- left_join(birdocc, birdevent, by=c("eventID"))
head(bird) # Now we've picked the matching lines from the birdevent and moved them over to the bird occ

# Isolate out species of interest 
unique(bird$vernacularName)

ambon_mams <- bird[bird$vernacularName %in% c("Walrus", "Bearded Seal", "Spotted Seal"), ] 

# Remove extraneous columns of info 
colnames(ambon_mams)
mams_new <- ambon_mams %>% dplyr::select(vernacularName,
                                         scientificName,
                                         individualCount,
                                         decimalLatitude,
                                         decimalLongitude,
                                         eventDate)


# Convert to spatial df ---------------------------------------------------

# Save lat/lon columns info into their own df to add in again later on
# or else they will disappear when converting to spatial object with st_as_sf
ambon_latlon <- mams_new %>% dplyr::select(vernacularName,
                                           decimalLatitude,
                                           decimalLongitude,
                                           eventDate)

# Check the CRS
st_crs(mams_new)
# This is non-spatial so the CRS is NA

# Convert data to a spatial object with CRS 4326 (since that is regular lat/lon) - this is still unprojected
mams_new_sp <- st_as_sf(mams_new, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326) 

# Project the data to Alaska Albers 3338
mams_new_sp <- mams_new_sp %>% st_transform(crs = 3338)

# Add back in lat/lon cols using left join to merge lat/lon data onto spatial data rather than spatial data onto lat/lon data
mams_sp <- mams_new_sp %>% left_join(ambon_latlon, by = "eventDate")

# Add a new column to specify data source so you can backtrack later if needed
mams_sp$source <- "AMBON Norseman II Cruise 2015"
# May also want to add other columns that help add information about the data source (e.g., vessel based, plane based, etc.)

# Remove extraneous columns again
mams_sp <- mams_sp %>% dplyr::select(vernacularName.x,
                                         scientificName,
                                         individualCount,
                                         decimalLatitude,
                                         decimalLongitude,
                                         eventDate,
                                         source)


# Figure out how R is classifying each column of data
lapply(mams_sp, class) # Fancy way

# Convert numeric columns to numeric if not already
mams_sp$decimalLatitude <- as.numeric(mams_sp$decimalLatitude)
mams_sp$decimalLongitude <- as.numeric(mams_sp$decimalLongitude)
mams_sp$individualCount <- as.integer(mams_sp$individualCount)

# Convert date to a standard format
# I find this website really helpful when I'm trying to remember codes
# If you're looking at sub-monthly data, be careful with time zones! In our case it doesn't matter. 
mams_sp$eventDate <- as.POSIXct(mams_sp$eventDate)

# Separate eventDate into year, month, day cols
## substr() takes the column name, starting position, and length of the string as arguments, returning
## the substring of the specific column
mams_sp$year <- substr(mams_sp$eventDate, 1, 4)
mams_sp$month <- substr(mams_sp$eventDate, 6, 7)
mams_sp$day <- substr(mams_sp$eventDate, 9, 10)

# Remove eventDate col and rearrange order of cols
mams_sp2 <- mams_sp %>% dplyr::select(scientificName,
                                        vernacularName.x,
                                        year,
                                        month,
                                        day,
                                        individualCount,
                                        decimalLatitude,
                                        decimalLongitude,
                                        source) %>% 
rename(scientificName = scientificName,
           commonName = vernacularName.x,
           groupSize = individualCount,
           decimalLat = decimalLatitude,
           decimalLon = decimalLongitude)

## Tada now you have a clean and tidy dataset to work with and merge

View(mams_sp2) # this is a regular dataframe

# Can save this data as an output as both shp and csv
# st_write(mams_sp2, "../Data_Processed/3_obis_ambon_norseman_2015.csv")
