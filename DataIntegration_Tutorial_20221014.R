# HELLO ATP TEAM! 

################################################################################
# TITLE: Wildlife Occurrence Data Integration Tutorial
# PURPOSE: The purpose of this script is to give an example of one way to 
# combine data from multiple sources to create a unified species occurrence data set. 
# AUTHOR: Kelly Kapsar
# CREATED: 2022-10-14
# LAST UPDATED ON: 2022-10-14
################################################################################

##### Preliminaries #####

# DATA NOTEBOOK 
# READ THE METADATA 

# Clear history 
rm(list=ls())

# Load libraries
library(sf) # For vector data (points, lines, polygons) 
library(tidyverse) # For data manipulation 

# Figure out file formats (most likely shp or csv)
# Find the info you're looking for
# Isolate that info 

#############################
##### Data set 1: AMBON ##### 
#############################

# Ended up downloading from here: https://www.sciencebase.gov/catalog/item/5e0e24b3e4b0b207aa13782b
# because it came in a csv format that I'm familiar with (not familiar with the Darwin format)

# Read in the data
birdocc <- read.csv("../Data_Raw/dwca-ambon_seabirds_2015-v11/dlog_2015_dwc_occurrence.csv")

# Take a look at what it looks like -- 1st 5 lines
head(birdocc)
colnames(birdocc)

# Look through the data -- make sure you understand what the columns mean and how this information was collected 
# Ex: Is each line one animal or one observation that could contain multiple animals? 
# It will also help to look at the metadata or readme files and the website for more info


# Looks like there aren't any locations. Hmm.... 
# Let's check some of the other files that came with it 
birdevent <- read.csv("../Data_Raw/dwca-ambon_seabirds_2015-v11/dlog_2015_dwc_event.csv")

head(birdevent)
# Here we go! This one has a lat and long. Looks like it's connected to the other data set via the shared "eventID" column 

# Let's join them together by that shared column 
bird <- left_join(birdocc, birdevent, by=c("eventID"))
head(bird) # Now we've picked the matching lines from the birdevent and moved them over to the bird occ

# Isolate out species of interest 
unique(bird$vernacularName)

# I'm not going to include "Seal spp." since we are going to model the species individual
# But in other instances you might want to 
marmams <- bird[bird$vernacularName %in% c("Walrus", "Bearded Seal", "Spotted Seal"),]
# Looks like we've got 78 observations of our focal species 

# Let's remove all of the extraneous columns of info 
colnames(marmams)
marmamsnew <- marmams %>% dplyr::select(vernacularName, scientificName, individualCount, decimalLatitude, decimalLongitude, eventDate)

# Add a new column to specify data source so you can backtrack later if needed
marmamsnew$source <- "AMBON 2015 Cruise"
# You may also want to add other columns that help add information about the data source (e.g., vessel based, plane based, etc.)

# Figure out how R is classifying each column of data
lapply(marmamsnew, class) # Fancy way
class(marmamsnew$vernacularName)

# Convert numeric columns to numeric 
marmamsnew$decimalLatitude <- as.numeric(marmamsnew$decimalLatitude)
marmamsnew$decimalLongitude <- as.numeric(marmamsnew$decimalLongitude)
marmamsnew$individualCount <- as.integer(marmamsnew$individualCount)

# Convert date to a standard format
# I find this website really helpful when I'm trying to remember codes
# If you're looking at sub-monthly data, be careful with time zones! In our case it doesn't matter. 
marmamsnew$eventDate <- as.POSIXct(marmamsnew$eventDate)

#############################
##### Data set 2: AMBON ##### 
#############################

# Load in data downloaded from the link you sent
obis <- read.csv("../Data_Raw/obis_seamap_dataset_1110_csv_71465/obis_seamap_dataset_1110_points.csv")
head(obis)

# Isolate out species of interest 
unique(obis$common_name)

# I'm not going to include "Seal spp." since we are going to model the species individual
# But in other instances you might want to 
obismams <- obis[obis$common_name %in% c("Walrus", "Bearded Seal", "Spotted Seal"),]

# Remove extraneous columns 
colnames(obismams)
obisnew <- obismams %>% dplyr::select(scientific_name, common_name, group_size, date_time, latitude, longitude)

# Add a new column to specify data source so you can backtrack later if needed
obisnew$source <- "Seismic 2013"
# You may also want to add other columns that help add information about the data source (e.g., vessel based, plane based, etc.)

# Figure out how R is classifying each column of data
lapply(obisnew, class)

# Convert numeric columns to numeric 
obisnew$latitude <- as.numeric(obisnew$latitude)
obisnew$longitude <- as.numeric(obisnew$longitude)
obisnew$group_size <- as.integer(obisnew$group_size)

# Convert date to a standard format
# I find this website really helpful when I'm trying to remember codes
# If you're looking at sub-monthly data, be careful with time zones! In our case it doesn't matter. 
obisnew$date_time <- as.POSIXct(obisnew$date_time)

#############################
##### Merge data sets ##### 
#############################

# Standardize column names
colnames(marmamsnew)
colnames(obisnew)

obisnew2 <- obisnew %>% rename(scientificName = scientific_name, 
                              vernacularName = common_name,
                              individualCount = group_size,
                              eventDate = date_time,
                              decimalLatitude = latitude, 
                              decimalLongitude = longitude)

alldf <- rbind(marmamsnew, obisnew2)

View(alldf)

# Make sure that capitalization, spelling, etc. are standardized between data sets. They have to be exactly the same for anything to work. 


alldf %>% group_by(scientificName) %>% summarize(indcount = sum(individualCount), obscount=n())
