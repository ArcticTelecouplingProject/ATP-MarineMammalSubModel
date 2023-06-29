# 31May2023
# Author: Sydney Waloven

# Source: NOAA
# Aerial Surveys of Arctic Marine Mammals (ASAMM) collected by Marine Mammal Laboratory,
# Bureau of Ocean Energy Management, and other agencies in the Bering, Chukchi,
# Beaufort Seas, and Amundsen Gulf from 1979 to 2019 and North Slope Borough, Alaska
# from 2020 to 2021
# https://www.ncei.noaa.gov/archive/archive-management-system/OAS/bin/prd/jquery/accession/download/39614

# Species isolated in this dataset: 

# This dataset was tidied and ready for data integration on 
# Last edited: 


# Libraries and Packages --------------------------------------------------

library(terra)
library(sf)
library(tidyverse)
library(readr)
library(xml2)

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
    Totalno
  ) %>%
  # Only keep rows that contain the following Species names
  dplyr::filter(
    Species %in% c("bearded seal", "unid pinniped", "small unid pinniped", "ringed seal", "walrus"),
    # Keep rows where the Sightcue column == body to only keep observations where the animal was ID'd by its full body
    Sightcue == "body"
  ) %>% 
  # Add new column to keep track of the source of observations when integrating with other datasets
  dplyr::mutate(
    source = "ASAMM 1979-2019"
  )

# Separate GMT_Minus8_DateTime column into year, month, day by using substr()
v0_mams$year <- substr(v0_mams$GMT_Minus8_DateTime, ) # add first and last value
v0_mams$month <- substr(v0_mams$GMT_Minus8_DateTime, ) # add first and last value
v0_mams$day <- substr(v0_mams$GMT_Minus8_DateTime, 1, 2)

# Transform to 3338



# 2015-2017 Data ----------------------------------------------------------

# Subset 2015-2017 dataframe 
v24_mams <- master_v24 %>%
  dplyr::select(
    GMT_Minus8_DateTime,
    ArcLat,
    ArcLong,
    Species,
    Sightcue,
    Totalno
  ) %>%
  # Only keep rows that contain the following Species names
  dplyr::filter(
    Species %in% c("bearded seal", "unid pinniped", "small unid pinniped", "ribbon seal", "walrus"),
    # Keep rows where the Sightcue column == body to only keep observations where the animal was ID'd by its full body
    Sightcue == "body"
  ) %>% 
  # Add new column to keep track of the source of observations when integrating with other datasets
  dplyr::mutate(
    source = "ASAMM 1979-2019"
  )


# 2018-2021 Data ----------------------------------------------------------

# Subset 2018-2021 dataframe 
v2_mams <- master_v2 %>%
  dplyr::select(
    GMT_Minus8_DateTime,
    ArcLat,
    ArcLong,
    Species,
    Sightcue,
    Totalno
  ) %>%
  # Only keep rows that contain the following Species names
  dplyr::filter(
    Species %in% c("bearded seal", "unid pinniped", "small unid pinniped", "walrus"),
    # Keep rows where the Sightcue column == body to only keep observations where the animal was ID'd by its full body
    Sightcue == "body"
  ) %>% 
  # Add new column to keep track of the source of observations when integrating with other datasets
  dplyr::mutate(
    source = "ASAMM 1979-2019"
  )





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
