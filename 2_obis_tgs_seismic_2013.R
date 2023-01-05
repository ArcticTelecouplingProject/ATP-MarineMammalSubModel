# 07Nov2022
# Author: Sydney Waloven

# TGS Seismic Survey Visual Sightings in Chukchi Sea 2013
# Source: https://obis.org/dataset/4b394534-8a6b-4bdb-bf1b-f07f983f9e37

# This dataset was tidied and ready for data integration on 07Nov2022.
# Last updated: 23Dec2022

# Libraries and Packages --------------------------------------------------

library(terra)
library(sf)
library(tidyverse)

# Clear history 
rm(list=ls())

# TGS Seismic Survey Chukchi 2013 data set #2 -----------------------------

# Read in TGS Seismic data
seismic_obis <- read.csv("../../ATP-MarineMammal-DataRaw/obis_seamap_dataset_1110_csv_71491/obis_seamap_dataset_1110_points.csv")

head(seismic_obis)

## Save lat/lon columns info into their own df to add in again later on
## or else they will disappear when converting to spatial object with st_as_sf
seismic_latlon <- seismic_obis %>% dplyr::select(common_name, latitude, longitude, date_time)
## Need to select lat/lons only for species of interest to be used later - needs to correspond
latlon_mams <- seismic_latlon[seismic_latlon$common_name %in% c("Walrus", "Bearded Seal", "Spotted Seal"), ]

# Check the CRS
st_crs(seismic_obis)
# This is non-spatial so the CRS is NA

# Convert data to a spatial object with CRS 4326 (since that is regular lat/lon) - this is still unprojected
seismic_obis_sp <- st_as_sf(seismic_obis, coords = c("longitude", "latitude"), crs = 4326) 

# Project the data to Alaska Albers 3338
seismic_obis_sp <- seismic_obis_sp %>% st_transform(crs = 3338)

# Did it work?
st_crs(seismic_obis_sp)

# Isolate species of interest in projected df
unique(seismic_obis_sp$common_name)

# Include species of interest (walrus, bearded seal, spotted seal)
seismic_mams <- seismic_obis_sp[seismic_obis_sp$common_name %in% c("Walrus", "Bearded Seal", "Spotted Seal"), ]

# Remove extraneous columns - using the select function to select which columns to keep in the new df
colnames(seismic_mams)
seismic_new <- seismic_mams %>% dplyr::select(scientific_name,
                                              common_name,
                                              group_size,
                                              date_time)


# Add a new column to specify data source
seismic_new$source <- "TGS Seismic 2013"

## Add back in latitude and longitude columns - needs to correspond with number of rows
## and species of interest 
# Merge latitude and longitude columns from latlon_mams to seismic_new
# Need to left join lat/lon data onto spatial data rather than spatial data onto lat/lon data
seismic_new2 <- seismic_new %>% left_join(latlon_mams, by = "date_time")


# Figure out how R is classifying each column of data
lapply(seismic_new2, class)


# Convert numeric columns to numeric if not already
# seismic_new2$latitude <- as.numeric(seismic_new2$latitude)
# seismic_new2$longitude <- as.numeric(seismic_new2$longitude)
# seismic_new2$group_size <- as.numeric(seismic_new2$group_size) # was previously as.integer but not sure why this needed to be an integer?

# Convert date to a standard format
## Be careful with time zones for sub-monthly data
seismic_new2$date_time <- as.POSIXct(seismic_new2$date_time)

# Separate date_time col into date and time cols
## substr() takes the column name, starting position, and length of the string as arguments, returning
## the substring of the specific column
seismic_new2$year <- substr(seismic_new2$date_time, 1, 4)
seismic_new2$month <- substr(seismic_new2$date_time, 6, 7)
seismic_new2$day <- substr(seismic_new2$date_time, 9, 10)

# Need to be able to sum how many observations are in each day, month, year

# Select only one of the common_name columns to include in the seismic_new2 df
seismic_new2 <- seismic_new2 %>% dplyr::select(scientific_name,
                                               common_name.x,
                                               year,
                                               month,
                                               day,
                                               group_size,
                                               latitude,
                                               longitude,
                                               source,
                                               geometry)


# Renaming column names from the seismic_new2 df to match the arcss_gbif dfs
seismic_merge <- seismic_new2 %>% rename(scientificName = scientific_name,
                                         commonName = common_name.x,
                                         groupSize = group_size,
                                         decimalLat = latitude,
                                         decimalLon = longitude)


## Woo now you have a tidy data set of all three species of interest to work with and merge!

View(seismic_merge)

# Save this df to a shp and csv for integration importing
# st_write(seismic_merge, "../Data_Processed/2_tgs_seismic_2013.shp")
