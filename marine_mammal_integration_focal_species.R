# Title: Marine Mammal Data Paper Integration Script
# Author: Sydney Waloven and Natalia Portales
# Date: 2024-07-10
# Description: This data set merges the following tidied data sets:

# 1-1_GBIF_queries_walrus
# 1-2_GBIF_queries_spotted_seal
# 1-3_GBIF_queries_bearded_seal
# 2_obis_tgs_seismic_2013
# 3_obis_ambon_norseman_2015
# 4_nsf_moore_mmw_2016
# 5_nsf_moore_mmw_2017
# 6_nsf_dbo_laurier_2019
# 7-1_nsf_dbo_laurier_2018
# 7-2_nsf_dbo_healy_2018
# 8_nmml_aerial_1979_2010
# 9_usgs_walrus_sex_age_comp_2013_2015
# 10_usgs_walrus_behavior_uas_2015
# 12_1_gbif_moudry
# 12_2_gbif_moudry
# 13_aerial_surveys_asamm
# 14_nmml_cetaceans_survey_1999
# 15_asti_marine_walrus
# 16_uam_collections_arctos
# 17_nmml_bering_survey_2000
# 18-1_nsf_mm_beringia_2008
# 18-2_nsf_mm_beringia_2008
# 19_nsf_mm_beringia_2007

# setup -------------------------------------------------------------------

# Load libraries
library(sf)
library(tidyverse)

# Clear environment history 
rm(list=ls())

# Import data -------------------------------------------------------------

# List individual data sets to the environment -----

# List all files
list.files("../Data_Processed/TidyScripts/", 
           pattern = ".csv$", 
           full.names = TRUE) %>%
  
  # Assign names
  set_names(~ basename(.) %>%
              tools::file_path_sans_ext() %>%
              gsub("^[^_]*_", "", .)) %>%
  
  # Read each CSV
  map(~ read_csv(.)) %>%
  
  # List individual items to the global environment
  list2env(.GlobalEnv)


# Read all data sets into one integrated df -----

# List all csvs in the TidyScripts folder and read in to one df
tidyscripts <- list.files("../Data_Processed/TidyScripts/",
                          pattern = ".csv$",
                          full.names = TRUE) %>% 
  read_csv()


# data tidying ------------------------------------------------------------


# Isolate occurrences of focal species
all_df_focal <- tidyscripts %>% 
  filter(scientificName %in% c("Odobenus rosmarus", 
                               "Phoca largha",
                               "Erignathus barbatus")) %>% 
  
  # Eliminate repeat occurrences
  distinct() %>% 
  
  # Remove NAs
  na.omit()

# Save this to a csv and shp
st_write(all_df_focal, "../Data_Processed/marine_mammal_integration/marine_mammal_integration_focal_species.csv")

st_write(all_df_focal, "../Data_Processed/marine_mammal_integration/marine_mammal_integration_focal_species.shp")


