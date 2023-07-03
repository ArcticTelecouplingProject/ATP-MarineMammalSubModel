# 31May2023
# Author: Sydney Waloven

# Source: NOAA
# Aerial Surveys of Arctic Marine Mammals (ASAMM) collected by Marine Mammal Laboratory,
# Bureau of Ocean Energy Management, and other agencies in the Bering, Chukchi,
# Beaufort Seas, and Amundsen Gulf from 1979 to 2019 and North Slope Borough, Alaska
# from 2020 to 2021
# https://www.ncei.noaa.gov/archive/archive-management-system/OAS/bin/prd/jquery/accession/download/39614

# Species isolated in this dataset: 

# This dataset was tidied and ready for data integration on 03Jul2023
# Last edited: 03Jul2023


# Libraries and Packages --------------------------------------------------

library(terra)
library(sf)
library(tidyverse)
library(readr)
library(xml2) # to read in metadata xml file 
library(lubridate) # to separate date into year, month, day columns

# Clear history 
rm(list=ls())


# ASAMM Surveys -----------------------------------------------------------

# Import raw walrus occurrence data
datadir <- "../../ATP-MarineMammal-DataRaw/"

# Save file path prefix
asamm <- "13_aerial_surveys_asamm/0039614/11.11/data/0-data/"


# Specify file path to Output files for relevant years

# 2012-2014
master_v0 <- st_read(paste0(datadir, asamm, "Aerial_Master_2012_2014_v0_32_Output.csv"))
# 2015-2017
master_v24 <- st_read(paste0(datadir, asamm, "Aerial_Master_2015_2017_v24_Output.csv"))
# 2018-2021
master_v2 <- st_read(paste0(datadir, asamm, "Aerial_Master_2018_2021_v2_Output.csv"))

# Get unique values of species column to see if the dfs contain species of interest
unique(master_v0$Species)
unique(master_v24$Species)
unique(master_v2$Species)

# Check out column names
colnames(master_v0)
colnames(master_v24)
colnames(master_v2)


# 2012-2014 Data ----------------------------------------------------------

# Subset 2012-2014 dataframe to get rid of extraneous columns
v0_mams <- master_v0 %>%
  dplyr::select(
    GMT_Minus8_DateTime,
    ArcLat,
    ArcLong,
    Species,
    Sightcue,
    Totalno,
  ) %>%
  # Only keep rows that contain the following Species names
  dplyr::filter(
    Species %in% c("bearded seal", "walrus"), # data also contains "unid pinniped", "small unid pinniped", "ringed seal"
    # Keep rows where the Sightcue column == body to only keep observations where the animal was ID'd by its full body
    Sightcue == "body"
  ) %>% 
  # Add new column to keep track of the source of observations when integrating with other datasets
  dplyr::mutate(
    source = "ASAMM 1979-2019"
  )

# Separate GMT_Minus8_DateTime column into year, month, day by using substr()
# mdy_hms() function is used to parse the date-time value, assuming the format
# "month/day/year hour:minute:second AM/PM". 
v0_datetime <- mdy_hms(v0_mams$GMT_Minus8_DateTime)
v0_mams$year <- year(v0_datetime)
v0_mams$month <- month(v0_datetime)
v0_mams$day <- day(v0_datetime)

# Add scientific name columns
v0_mams$scientificName[v0_mams$Species == "walrus"] <- "Odobenus rosmarus divergens"
v0_mams$scientificName[v0_mams$Species == "bearded seal"] <- "Erignathus barbatus"

# Rename walrus and bearded seal to Walrus and Bearded Seal for consistency with
# other dataframes when integrating
v0_mams$Species[v0_mams$Species == "walrus"] <- "Walrus"
v0_mams$Species[v0_mams$Species == "bearded seal"] <- "Bearded Seal"

# Add methodObservation column and fill it with Aerial survey
v0_mams$methodObservation <- "Aerial survey"


# Convert data to a spatial object with CRS 4326 (since this is regular lat/lon) - this is still unprojected
v0_sp <- st_as_sf(v0_mams, coords = c("ArcLong", "ArcLat"), crs = 4326, remove=FALSE) %>%
  # Project the data to Alaska Albers CRS = 3338
  st_transform(
  crs = 3338
)

# Remove extraneous cols, rearrange, and rename
v0_sp <- v0_sp %>% dplyr::select(scientificName,
                                      Species,
                                      year,
                                      month,
                                      day,
                                      Totalno,
                                      ArcLat,
                                      ArcLong,
                                      methodObservation,
                                      source) %>% 
  rename(commonName = Species,
         groupSize = Totalno,
         decimalLat = ArcLat,
         decimalLon = ArcLong,
         )

# 2015-2017 Data ----------------------------------------------------------

# Subset 2012-2014 dataframe to get rid of extraneous columns
v24_mams <- master_v24 %>%
  dplyr::select(
    GMT_Minus8_DateTime,
    ArcLat,
    ArcLong,
    Species,
    Sightcue,
    Totalno,
  ) %>%
  # Only keep rows that contain the following Species names
  dplyr::filter(
    Species %in% c("bearded seal", "walrus"), # data also contains "unid pinniped", "small unid pinniped", "ringed seal"
    # Keep rows where the Sightcue column == body to only keep observations where the animal was ID'd by its full body
    Sightcue == "body"
  ) %>% 
  # Add new column to keep track of the source of observations when integrating with other datasets
  dplyr::mutate(
    source = "ASAMM 1979-2019"
  )

# Separate GMT_Minus8_DateTime column into year, month, day by using substr()
# mdy_hms() function is used to parse the date-time value, assuming the format
# "month/day/year hour:minute:second AM/PM". 
v24_datetime <- mdy_hms(v24_mams$GMT_Minus8_DateTime)
v24_mams$year <- year(v24_datetime)
v24_mams$month <- month(v24_datetime)
v24_mams$day <- day(v24_datetime)

# Add scientific name columns
v24_mams$scientificName[v24_mams$Species == "walrus"] <- "Odobenus rosmarus divergens"
v24_mams$scientificName[v24_mams$Species == "bearded seal"] <- "Erignathus barbatus"

# Rename walrus and bearded seal to Walrus and Bearded Seal for consistency with
# other dataframes when integrating
v24_mams$Species[v24_mams$Species == "walrus"] <- "Walrus"
v24_mams$Species[v24_mams$Species == "bearded seal"] <- "Bearded Seal"

# Add methodObservation column and fill it with Aerial survey
v24_mams$methodObservation <- "Aerial survey"


# Convert data to a spatial object with CRS 4326 (since this is regular lat/lon) - this is still unprojected
v24_sp <- st_as_sf(v24_mams, coords = c("ArcLong", "ArcLat"), crs = 4326, remove=FALSE) %>%
  # Project the data to Alaska Albers CRS = 3338
  st_transform(
    crs = 3338
  )

# Remove extraneous cols, rearrange, and rename
v24_sp <- v24_sp %>% dplyr::select(scientificName,
                                 Species,
                                 year,
                                 month,
                                 day,
                                 Totalno,
                                 ArcLat,
                                 ArcLong,
                                 methodObservation,
                                 source) %>% 
  rename(commonName = Species,
         groupSize = Totalno,
         decimalLat = ArcLat,
         decimalLon = ArcLong,
  )

# 2018-2021 Data ----------------------------------------------------------

# Subset 2012-2014 dataframe to get rid of extraneous columns
v2_mams <- master_v2 %>%
  dplyr::select(
    GMT_Minus8_DateTime,
    ArcLat,
    ArcLong,
    Species,
    Sightcue,
    Totalno,
  ) %>%
  # Only keep rows that contain the following Species names
  dplyr::filter(
    Species %in% c("bearded seal", "walrus"), # data also contains "unid pinniped", "small unid pinniped", "ringed seal"
    # Keep rows where the Sightcue column == body to only keep observations where the animal was ID'd by its full body
    Sightcue == "body"
  ) %>% 
  # Add new column to keep track of the source of observations when integrating with other datasets
  dplyr::mutate(
    source = "ASAMM 1979-2019"
  )

# Separate GMT_Minus8_DateTime column into year, month, day by using substr()
# mdy_hms() function is used to parse the date-time value, assuming the format
# "month/day/year hour:minute:second AM/PM". 
v2_datetime <- mdy_hms(v2_mams$GMT_Minus8_DateTime)
v2_mams$year <- year(v2_datetime)
v2_mams$month <- month(v2_datetime)
v2_mams$day <- day(v2_datetime)

# Add scientific name columns
v2_mams$scientificName[v2_mams$Species == "walrus"] <- "Odobenus rosmarus divergens"
v2_mams$scientificName[v2_mams$Species == "bearded seal"] <- "Erignathus barbatus"

# Rename walrus and bearded seal to Walrus and Bearded Seal for consistency with
# other dataframes when integrating
v2_mams$Species[v2_mams$Species == "walrus"] <- "Walrus"
v2_mams$Species[v2_mams$Species == "bearded seal"] <- "Bearded Seal"

# Add methodObservation column and fill it with Aerial survey
v2_mams$methodObservation <- "Aerial survey"


# Convert data to a spatial object with CRS 4326 (since this is regular lat/lon) - this is still unprojected
v2_sp <- st_as_sf(v2_mams, coords = c("ArcLong", "ArcLat"), crs = 4326, remove=FALSE) %>%
  # Project the data to Alaska Albers CRS = 3338
  st_transform(
    crs = 3338
  )

# Remove extraneous cols, rearrange, and rename
v2_sp <- v2_sp %>% dplyr::select(scientificName,
                                   Species,
                                   year,
                                   month,
                                   day,
                                   Totalno,
                                   ArcLat,
                                   ArcLong,
                                   methodObservation,
                                   source) %>% 
  rename(commonName = Species,
         groupSize = Totalno,
         decimalLat = ArcLat,
         decimalLon = ArcLong,
  )


# Save data ---------------------------------------------------------------

# Integrate the three time periods and save as one csv and shp
all_asamm <- rbind(v0_sp, v24_sp, v2_sp)

# Eliminate repeat records
unique_asamm <- distinct(all_asamm)

# Remove NAs
unique_omit <- na.omit(unique_asamm)

# Save to csv and shp
st_write(unique_omit, "../Data_Processed/13_aerial_surveys_asamm/13_aerial_surveys_asamm.shp")




# COMIDA and BWASP --------------------------------------------------------

# Look at data of COMIDA (Chukchi Offshore Monitoring in Drilling Area) and 
# BWASP (Bowhead Whale Aerial Survey Project)
comida <- st_read(paste0(datadir, asamm, "ASAMM_Blocks/COMIDA23.shp"))
bwasp <- st_read(paste0(datadir, asamm, "ASAMM_Blocks/BWASPblocks_ExclBarrierIs_ManyPts.shp"))

plot(comida)
plot(bwasp)




# Graveyard ---------------------------------------------------------------

# # Read in metadata
# xml_metadata <- read_xml(paste0(datadir, asamm, "ASAMM_Metadata_2021.xml"))
# xml_structure(xml_metadata)
# 
# # Extract contents of xml file
# elements <- xml_find_all(xml_metadata, "//desired/element")
