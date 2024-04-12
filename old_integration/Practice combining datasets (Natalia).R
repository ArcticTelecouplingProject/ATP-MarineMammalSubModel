################################################################################
# TITLE: Practice combining datasets (Natalia) - Occurrence Data Integration Tutorial
# PURPOSE: The purpose of this script is to practice combining data from multiple sources 
# to create a unified species occurrence data set from Kelly's data integration tutorial. 
# AUTHOR: Natalia Portales (based on work by Kelly Kapsar)
# CREATED: 2022-11-13
# LAST UPDATED ON: 2022-11-13
#
################################################################################

##### Preliminaries #####

# DATA NOTEBOOK 
# READ THE METADATA 

# Clear history 
rm(list=ls())

##trouble shooting tidyverse errors##ignore
install.packages("rlang")
install.packages("cli")
.libPaths()
install.packages("tidyverse")

##Installing tidyverse solution
install.packages("devtools")
devtools::install_github("tidyverse/tidyverse")
1

# Load libraries
library(sf) # For vector data (points, lines, polygons) 
library(tidyverse) # For daa manipulation 

# Figure out file formats (most likely shp or csv)
# Find the info you're looking for
# Isolate that info 

setwd("~/R/Data Integration (marine mammal datasets)/Code")

#############################
##### Data set 1: USGS ##### 
#############################

# Ended up downloading from here: https://www.sciencebase.gov/catalog/item/5e0e24b3e4b0b207aa13782b
# because it came in a csv format that I'm familiar with (not familiar with the Darwin format)

# Read in the data
walrushaulout <- read.csv("../Data_Raw/walrus_hauloutGroupComp_battaile_2013_2015 (1)/walrus_hauloutGroupComp_battaile_2013_2015.csv")

# Take a look at what it looks like -- 1st 5 lines
head(walrushaulout)
colnames(walrushaulout)

# Look through the data -- make sure you understand what the columns mean and how this information was collected 
# Ex: Is each line one animal or one observation that could contain multiple animals? 
# It will also help to look at the metadata or readme files and the website for more info

# Let's remove all of the extraneous columns of info 
colnames(walrushaulout)
walrushauloutnew <- walrushaulout %>% dplyr::select(GroupId, Latitude, Longitude, GroupSize, DistanceToGroup)

# Add a new column to specify data source so you can backtrack later if needed
walrushauloutnew$source <- "USGS 2013-2015 walrus"
# You may also want to add other columns that help add information about the data source (e.g., vessel based, plane based, etc.)

# Figure out how R is classifying each column of data
lapply(walrushauloutnew, class)

# Convert numeric columns to numeric 
walrushauloutnew$Latitude <- as.numeric(walrushauloutnew$Latitude)
walrushauloutnew$Longitude <- as.numeric(walrushauloutnew$Longitude)
walrushauloutnew$GroupSize <- as.integer(walrushauloutnew$GroupSize)

# Fixing date format and separating GroupId col into year, month, day cols
# substr() takes the column name, starting position, and length of the string as arguments
# substring of the specific column
walrushauloutnew$Year <- substr(walrushauloutnew$GroupId, 4, 7)
walrushauloutnew$Month <- substr(walrushauloutnew$GroupId, 9, 10)
walrushauloutnew$Day <- substr(walrushauloutnew$GroupId, 12, 13)

#checking if substringing worked
head(walrushauloutnew)
colnames(walrushauloutnew)

# Add a scientific name and common name column to walrushauloutnew and fill with "Odobenus rosmarus divergens" and "Pacific walrus"
walrushauloutnew$ScientificName <- "Odobenus rosmarus divergens"
walrushauloutnew$CommonName <- "Pacific walrus"

# Let's remove all of the extraneous columns of info again
colnames(walrushauloutnew)
walrushauloutmerge <- walrushauloutnew %>% dplyr::select(ScientificName, CommonName, Year, Month, Day, GroupSize, Latitude, Longitude, DistanceToGroup)

#checking if edits all worked
head(walrushauloutmerge)
colnames(walrushauloutmerge)
View(walrushauloutmerge)


#############################
##### Data set 2: USGS ##### 
#############################

# Load in data downloaded from the link you sent
Walrusbehavior <- read.csv("../Data_Raw/walrus_behavior_UAS_chukchi_battaile_2015 (1)/walrus_environConditionsUAS_chukchi_battaile_2015.csv")
head(Walrusbehavior)

# Remove extraneous columns 
colnames(Walrusbehavior)
Walrusbehaviornew <- Walrusbehavior %>% dplyr::select(Flight, Group_Size, Latitude, Longitude, Vessel_Distance)

# Add a new column to specify data source so you can backtrack later if needed
Walrusbehaviornew$source <- "USGS 2015 Walrus"
# You may also want to add other columns that help add information about the data source (e.g., vessel based, plane based, etc.)

# Figure out how R is classifying each column of data
lapply(Walrusbehaviornew, class)

# Convert numeric columns to numeric 
Walrusbehaviornew$latitude <- as.numeric(Walrusbehaviornew$Latitude)
Walrusbehaviornew$longitude <- as.numeric(Walrusbehaviornew$Longitude)
Walrusbehaviornew$group_size <- as.integer(Walrusbehaviornew$Group_Size)

# Fixing date format and separating GroupId col into year, month, day cols
# substr() takes the column name, starting position, and length of the string as arguments
# substring of the specific column
Walrusbehaviornew$Year <- substr(Walrusbehaviornew$Flight, 1, 4)
Walrusbehaviornew$Month <- substr(Walrusbehaviornew$Flight, 5, 6)
Walrusbehaviornew$Day <- substr(Walrusbehaviornew$Flight, 7, 8)

# Add a scientific name and common name column to walrushauloutnew and fill with "Odobenus rosmarus divergens" and "Pacific walrus"
Walrusbehaviornew$ScientificName <- "Odobenus rosmarus divergens"
Walrusbehaviornew$CommonName <- "Pacific walrus"

# Let's remove all of the extraneous columns of info again
colnames(Walrusbehaviornew)
Walrusbehaviornewmerge <- Walrusbehaviornew %>% dplyr::select(ScientificName, CommonName, Year, Month, Day, Group_Size, Latitude, Longitude, Vessel_Distance)

#checking if edits all worked
head(Walrusbehaviornewmerge)
colnames(Walrusbehaviornewmerge)
View(Walrusbehaviornewmerge)

#############################
##### Merge data sets ##### 
#############################

# Standardize column names
colnames(walrushauloutmerge)
colnames(Walrusbehaviornewmerge)

Walrusbehaviornewmerge2 <- Walrusbehaviornewmerge %>% dplyr::rename(GroupSize = Group_Size,
                                                                    DistanceToGroup = Vessel_Distance)
# Check work
colnames(Walrusbehaviornewmerge2)

alldf <- rbind(walrushauloutmerge, Walrusbehaviornewmerge2)

View(alldf)

# Make sure that capitalization, spelling, etc. are standardized between data sets. They have to be exactly the same for anything to work. 


alldf %>% group_by(scientificName) %>% summarize(indcount = sum(individualCount), obscount=n())

<- save.csv
