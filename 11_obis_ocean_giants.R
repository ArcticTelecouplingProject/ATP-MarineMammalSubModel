# 01Feb2023
# Author: Sydney Waloven

# Source: OBIS
# This dataset was obtained from Sizing Ocean Giants
# https://obis.org/dataset/4a058eda-7edc-400b-a5a2-49dc8c193412

# Species isolated in this dataset: walrus
# This dataset doesn't seem to have information for occurrence dates so it is up to us
# whether we want to include this in our integrated dataset.

# Last edited: 01Feb2023


# Libraries and Packages --------------------------------------------------

library(terra)
library(sf)
library(tidyverse)

# Clear history 
rm(list=ls())



# USGS Walrus UAS Overflights 2015 ----------------------------------------

# Import raw walrus occurrence data
datadir <- "C:/Users/Natalia Portales/Michigan State University/Waloven, Sydney - ATP-MarineMammal-DataRaw/"
datadir <- "../../ATP-MarineMammal-DataRaw/"

obis_giants <- read.csv(paste0(datadir,"11_obis_ocean_giants/occurance-dwca-sizinggiants-v1.8 (1).csv"))

head(obis_giants)

# Remove extraneous columns of info 
colnames(obis_giants)
obis_ocean <- obis_giants %>% dplyr::select(occurrenceID,
                                            scientificName,
                                            year,
                                            month,
                                            day,
                                            decimalLatitude,
                                            decimalLongitude,
                                            waterBody,
                                            individualCount)


# Filter by scientificName for walrus
obis_walrus <- obis_ocean[obis_ocean$scientificName %in% "Odobenus rosmarus divergens", ]

