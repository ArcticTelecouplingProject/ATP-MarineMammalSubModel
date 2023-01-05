# 05Jan2022
# Author: Sydney Waloven

# Source: OBIS
# This dataset was obtained from: AMBON Chukchi Sea research cruse on the Norseman II
# https://www.sciencebase.gov/catalog/item/5e0e24b3e4b0b207aa13782b
# Corresponds with Data_Raw folder: obis_ambon_norseman_2015

# Species of interest in this script is the spotted seal.

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

seals <- bird[bird$vernacularName %in% c("Bearded Seal", "Spotted Seal"),] # can also add walrus here if isolating for pacific walrus

# Remove extraneous columns of info 
colnames(seals)
sealsnew <- seals %>% dplyr::select(vernacularName, scientificName, individualCount, decimalLatitude, decimalLongitude, eventDate)

# Add a new column to specify data source so you can backtrack later if needed
sealsnew$source <- "AMBON Norseman II Cruise 2015"
# May also want to add other columns that help add information about the data source (e.g., vessel based, plane based, etc.)

# Figure out how R is classifying each column of data
lapply(sealsnew, class) # Fancy way

# Convert numeric columns to numeric if not already
sealsnew$decimalLatitude <- as.numeric(sealsnew$decimalLatitude)
sealsnew$decimalLongitude <- as.numeric(sealsnew$decimalLongitude)
sealsnew$individualCount <- as.integer(sealsnew$individualCount)

# Convert date to a standard format
# I find this website really helpful when I'm trying to remember codes
# If you're looking at sub-monthly data, be careful with time zones! In our case it doesn't matter. 
sealsnew$eventDate <- as.POSIXct(sealsnew$eventDate)

# Separate eventDate into year, month, day cols
## substr() takes the column name, starting position, and length of the string as arguments, returning
## the substring of the specific column
sealsnew$year <- substr(sealsnew$eventDate, 1, 4)
sealsnew$month <- substr(sealsnew$eventDate, 6, 7)
sealsnew$day <- substr(sealsnew$eventDate, 9, 10)

# Remove eventDate col and rearrange order of cols
sealsnew2 <- sealsnew %>% dplyr::select(scientificName,
                                        vernacularName,
                                        year,
                                        month,
                                        day,
                                        individualCount,
                                        decimalLatitude,
                                        decimalLongitude,
                                        source)

# Rename columns to match for integration
sealsnew2 <- sealsnew2 %>% rename(scientificName = scientificName,
                                commonName = vernacularName,
                                groupSize = individualCount,
                                decimalLat = decimalLatitude,
                                decimalLon = decimalLongitude)
                                
## Tada now you have a clean and tidy dataset to work with and merge

View(sealsnew2) # this is a regular dataframe
