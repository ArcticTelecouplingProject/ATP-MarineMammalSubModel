# 15Feb2023
# Author: Natalia Portales

# Source: GBIF
# Quality and usability challenges of global marine biodiversity databases: An example for marine mammal data
# https://www.gbif.org/occurrence/download/0024533-190918142434337

# Species isolated in this dataset: walrus

# This dataset was tidied and ready for data integration on 15Feb2023
# Last edited: 15Feb2023


# Libraries and Packages --------------------------------------------------

library(terra)
library(sf)
library(tidyverse)

# Clear history 
rm(list=ls())



# Walrus GBIF Database 1 ----------------------------------------

# Import raw walrus occurrence data
datadir <- "C:/Users/Natalia Portales/Michigan State University/Waloven, Sydney - ATP-MarineMammal-DataRaw/"
datadir <- "../../ATP-MarineMammal-DataRaw/"

gbif_walrus_1 <- read.csv(paste0(datadir,"12_gbif_Moudrý/1-0024533-190918142434337.csv"))

head(gbif_walrus_1)

# Remove extraneous columns of info 
colnames(gbif_walrus_1)
gbif_walrus_1_new <- gbif_walrus_1 %>% dplyr::select(scientificName,
                                                 individualCount,
                                                 decimalLatitude,
                                                 decimalLongitude,
                                                 day,
                                                 month,
                                                 year,
                                                 basisOfRecord)
# Isolate Pacific Walrus by scientificName
gbif_pwalrus_1_new <- gbif_walrus_1_new[gbif_walrus_1_new$scientificName %in% c("Odobenus rosmarus divergens (Illiger, 1815)"),]

#Change the scientific naming convention
gbif_pwalrus_1_new$scientificName[gbif_pwalrus_1_new$scientificName == "Odobenus rosmarus divergens (Illiger, 1815)"] <- 'Odobenus rosmarus divergens'

# Convert to spatial df ---------------------------------------------------

# Add commonName column
gbif_pwalrus_1_new$commonName <- "Walrus"

# Save lat/lon columns info into their own df to add in again later on
# or else they will disappear when converting to spatial object with st_as_sf
gbif_latlon <- gbif_pwalrus_1_new %>% dplyr::select(scientificName,
                                                   decimalLatitude,
                                                   decimalLongitude,
                                                   day)

# Check the CRS
st_crs(gbif_pwalrus_1_new) # this is non-spatial so the CRS is NA

# Convert data to a spatial object with CRS 4326 (since that is regular lat/lon) - this is still unprojected
gbif_pwalrus_1_new_sp <- st_as_sf(gbif_pwalrus_1_new, coords = c("decimalLatitude", "decimalLongitude"), crs = 4326) 

# Project the data to Alaska Albers 3338
gbif_pwalrus_1_new_sp <- gbif_pwalrus_1_new_sp %>% st_transform(crs = 3338)

# Add back in lat/lon cols using left join to merge lat/lon data onto spatial data rather than spatial data onto lat/lon data
pwalrus_1_sp <- gbif_pwalrus_1_new_sp %>% left_join(gbif_latlon, by = "day")

# Add a new column to specify data source so you can backtrack later if needed
pwalrus_1_sp$source <- "Moudrý GBIF Marine Mammal Walrus Data"
# May also want to add other columns that help add information about the data source (e.g., vessel based, plane based, etc.)
# Rename basisOfRec column to methodObservation column
pwalrus_1_sp <- pwalrus_1_sp %>% rename("methodObservation" = "basisOfRecord")
# Rename values of method column
pwalrus_1_sp$methodObservation[pwalrus_1_sp$methodObservation == "HUMAN_OBSERVATION"] <- 'Human observation'
pwalrus_1_sp$methodObservation[pwalrus_1_sp$methodObservation == "PRESERVED_SPECIMEN"] <- 'Preserved specimen'

# Remove extraneous columns again, rearrange, and name
pwalrus_1_sp2 <- pwalrus_1_sp %>% dplyr::select(scientificName.x,
                                          commonName,
                                          year,
                                          month,
                                          day,
                                          individualCount,
                                          decimalLatitude,
                                          decimalLongitude,
                                          methodObservation,
                                          source) %>% 
  rename(scientificName = scientificName.x,
         groupSize = individualCount,
         decimalLat = decimalLatitude,
         decimalLon = decimalLongitude)


## Tada now you have a clean and tidy dataset to work with and merge

View(pwalrus_1_sp2)

# Can save this data as an output as both shp and csv
st_write(pwalrus_1_sp2, "../Data_Processed/12_gbif_Moudrý/1-gbif_database_walrus.shp")
