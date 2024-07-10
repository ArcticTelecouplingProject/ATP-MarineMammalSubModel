# 23Mar2023
# Author: Natalia Portales

# Source: GBIF
# Quality and usability challenges of global marine biodiversity databases: An example for marine mammal data
# https://www.gbif.org/occurrence/download/0024533-190918142434337

# Species isolated in this dataset: walrus

# This dataset was tidied and ready for data integration on 23Mar2023
# Last edited: 26Mar2023


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

gbif_walrus_2 <- read.csv(paste0(datadir,"12_gbif_moudry/2-0003097-150811131857512.csv"))

head(gbif_walrus_2)

# Remove extraneous columns of info 
colnames(gbif_walrus_2)
gbif_walrus_2_new <- gbif_walrus_2 %>% dplyr::select(scientificname,
                                                     decimallatitude,
                                                     decimallongitude,
                                                     day,
                                                     month,
                                                     year,
                                                     basisofrecord)
# Isolate Pacific Walrus by scientificName
gbif_pwalrus_2_new <- gbif_walrus_2_new[gbif_walrus_2_new$scientificname %in% c("Odobenus rosmarus divergens (Illiger, 1815)", "Odobenus rosmarus (Linnaeus, 1758)"),]

#Change the scientific naming convention
gbif_pwalrus_2_new$scientificname[gbif_pwalrus_2_new$scientificname == "Odobenus rosmarus divergens (Illiger, 1815)"] <- 'Odobenus rosmarus divergens'

#Change the scientific naming convention
gbif_pwalrus_2_new$scientificname[gbif_pwalrus_2_new$scientificname == "Odobenus rosmarus (Linnaeus, 1758)"] <- 'Odobenus rosmarus'

# Convert to spatial df ---------------------------------------------------

# Add commonName column
gbif_pwalrus_2_new$commonName <- "Walrus"

# Save lat/lon columns info into their own df to add in again later on
# or else they will disappear when converting to spatial object with st_as_sf
gbif_latlon <- gbif_pwalrus_2_new %>% dplyr::select(scientificname,
                                                    decimallatitude,
                                                    decimallongitude,
                                                    day)

# Check the CRS
st_crs(gbif_pwalrus_2_new) # this is non-spatial so the CRS is NA

# Convert data to a spatial object with CRS 4326 (since that is regular lat/lon) - this is still unprojected
# Make sure longitude is listed first (longitude col, latitude col)
gbif_pwalrus_2_new_sp <- st_as_sf(gbif_pwalrus_2_new, coords = c("decimallongitude", "decimallatitude"), crs = 4326) 

# Project the data to Alaska Albers 3338
gbif_pwalrus_2_new_sp <- gbif_pwalrus_2_new_sp %>% st_transform(crs = 3338)

# Add back in lat/lon cols using left join to merge lat/lon data onto spatial data rather than spatial data onto lat/lon data
pwalrus_2_sp <- gbif_pwalrus_2_new_sp %>% left_join(gbif_latlon, by = "day")

# Add a new column to specify data source so you can backtrack later if needed
pwalrus_2_sp$source <- "Moudry GBIF Marine Mammal Walrus Data"

# May also want to add other columns that help add information about the data source (e.g., vessel based, plane based, etc.)
# Rename basisOfRec column to methodObservation column
pwalrus_2_sp <- pwalrus_2_sp %>% rename("methodObservation" = "basisofrecord")
# Rename values of method column
pwalrus_2_sp$methodObservation[pwalrus_2_sp$methodObservation == "HUMAN_OBSERVATION"] <- 'Human observation'
pwalrus_2_sp$methodObservation[pwalrus_2_sp$methodObservation == "PRESERVED_SPECIMEN"] <- 'Preserved specimen'
pwalrus_2_sp$methodObservation[pwalrus_2_sp$methodObservation == "MACHINE_OBSERVATION"] <- 'Machine observation'
pwalrus_2_sp$methodObservation[pwalrus_2_sp$methodObservation == "OBSERVATION"] <- 'Observation'
pwalrus_2_sp$methodObservation[pwalrus_2_sp$methodObservation == "UNKNOWN"] <- 'NA'

# Missing a groupSize column in the walrus_gbif_merge df??
# Add groupSize column to walrus_gbif_merge df based on whether there are no NAs in the year, month, day cols
## Reference for code:
## https://www.geeksforgeeks.org/how-to-add-a-column-based-on-other-columns-in-r-dataframe/
pwalrus_2_merge <- transform(pwalrus_2_sp, groupSize = ifelse
                               (is.na(year) & is.na(month) & is.na(day), 0, 1))
# Right now this is marking records with no YMD as 0
# Decide whether to mark records with an NA in any part of the YMD as 0?

# Remove extraneous columns again, rearrange, and name
pwalrus_2_merge <- pwalrus_2_merge %>% dplyr::select(scientificname.x,
                                                commonName,
                                                year,
                                                month,
                                                day,
                                                groupSize,
                                                decimallatitude,
                                                decimallongitude,
                                                methodObservation,
                                                source) %>% 
  rename(scientificName = scientificname.x,
         decimalLat = decimallatitude,
         decimalLon = decimallongitude)

# Remove rows that contain NAs in any of the columns
# https://statisticsglobe.com/r-remove-data-frame-rows-with-some-or-all-na
pwalrus_2_merge <- na.omit(pwalrus_2_merge)

## Tada now you have a clean and tidy dataset to work with and merge

View(pwalrus_2_merge)

# Can save this data as an output as both shp and csv
# st_write(pwalrus_2_merge, "../Data_Processed/12-2_gbif_moudry/12-2_gbif_moudry.csv")
