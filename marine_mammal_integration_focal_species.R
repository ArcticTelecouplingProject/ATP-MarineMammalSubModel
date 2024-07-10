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

# Import tidied data sets from Data_Processed - you will need to go through and run the individual tidy scripts to save the outputs into your Data Processed folder in order to proceed with this section, etc.
# Relative paths should work here since the output should be saved locally.

# 1-1_tidy_GBIF_queries_walrus
gbif_walrus <- st_read("../Data_Processed/1-1_tidy_GBIF_queries_walrus/1-1_tidy_GBIF_queries_walrus.csv")

# 1-2_tidy_GBIF_queries_spotted_seal
gbif_spotted <- st_read("../Data_Processed/1-2_tidy_GBIF_queries_spotted_seal/1-2_tidy_GBIF_queries_spotted_seal.csv")

# 1-3_tidy_GBIF_queries_bearded_seal
gbif_bearded <- st_read("../Data_Processed/1-3_tidy_GBIF_queries_bearded_seal/1-3_tidy_GBIF_queries_bearded_seal.csv")

# 2-1_tidy_obis_tgs_seismic_2013
obis_tgs <- st_read("../Data_Processed/2_obis_tgs_seismic_2013/2-1_tidy_obis_tgs_seismic_2013.csv")

# 3-1_tidy_obis_ambon_norseman_2015
obis_ambon <- st_read("../Data_Processed/3_obis_ambon_norseman_2015/3-1_tidy_obis_ambon_norseman_2015.csv")

# 4-1_tidy_nsf_moore_mmw_2016
mmwatch_2016 <- st_read("../Data_Processed/4_nsf_moore_mmw_2016/4-1_tidy_nsf_moore_mmw_2016.csv")

# 5-1_tidy_nsf_moore_mmw_2017
mmwatch_2017 <- st_read("../Data_Processed/5_nsf_moore_mmw_2017/5-1_tidy_nsf_moore_mmw_2017.csv")

# 6-1_tidy_nsf_dbo_laurier_2019
dbo_laurier_2019 <- st_read("../Data_Processed/6_nsf_dbo_laurier_2019/6-1_tidy_nsf_dbo_laurier_2019.csv")

# 7-1-1_tidy_nsf_dbo_laurier_2018
dbo_laurier_2018 <- st_read("../Data_Processed/7-1_nsf_dbo_laurier_2018/7-1-1_tidy_nsf_dbo_laurier_2018.csv")

# 7-2-1_tidy_nsf_dbo_healy_2018
dbo_healy_2018 <- st_read("../Data_Processed/7-2_nsf_dbo_healy_2018/7-2-1_tidy_nsf_dbo_healy_2018.csv")

# 8-1_tidy_nmml_aerial_1979_2010
nmml_aerial_1979 <- st_read("../Data_Processed/8_nmml_aerial_1979_2010/8-1_tidy_nmml_aerial_1979_2010.csv")

# 9-1_tidy_usgs_walrus_sex_age_comp_2013_2015
usgs_comp <- st_read("../Data_Processed/9_usgs_walrus_sex_age_comp_2013_2015/9-1_tidy_usgs_walrus_sex_age_comp_2013_2015.csv")

# 10-1_tidy_usgs_walrus_behavior_uas_2015
usgs_behavior <- st_read("../Data_Processed/10_usgs_walrus_behavior_uas_2015/10-1_tidy_usgs_walrus_behavior_uas_2015.csv")

# 12-1-1_tidy_gbif_moudry
gbif_moudry_1 <- st_read("../Data_Processed/12-1_gbif_moudry/12-1-1_tidy_gbif_moudry.csv")

# 12-2-1_tidy_gbif_moudry
gbif_moudry_2 <- st_read("../Data_Processed/12-2_gbif_moudry/12-2-1_tidy_gbif_moudry.csv")

# 13-1_tidy_aerial_surveys_asamm
asamm <- st_read("../Data_Processed/13_aerial_surveys_asamm/13-1_tidy_aerial_surveys_asamm.csv")

# 14_nmml_cetaceans_survey_1999
nmml_aerial_1999 <- st_read("../Data_Processed/14_nmml_cetacean_survey_1999.csv")

# 15_asti_marine_walrus
asti_walrus <- st_read("../Data_Processed/14_nmml_cetacean_survey_1999.csv")

# 16_uam_collections_arctos
uam_arctos <- st_read("../Data_Processed/14_nmml_cetacean_survey_1999.csv")

# 17_nmml_bering_survey_2000
nmml_2000 <- st_read("../Data_Processed/14_nmml_cetacean_survey_1999.csv")

# 18_nsf_mm_beringia_2008
mm_healy_2008 <- st_read("../Data_Processed/14_nmml_cetacean_survey_1999.csv")

# 19_nsf_mm_beringia_2007 
mm_healy_2007 <- st_read("../Data_Processed/14_nmml_cetacean_survey_1999.csv")

# Merge data sets ---------------------------------------------------------------

# Merge to create all_df
all_df <- rbind(gbif, 
                obis_tgs, 
                obis_ambon,
                mmwatch_2016, 
                mmwatch_2017, 
                dbo_laurier_2019,
                dbo_laurier_2018, 
                dbo_healy_2018, 
                nmml_aerial, 
                usgs_comp, 
                usgs_behavior, 
                gbif_moudry_1, 
                gbif_moudry_2, 
                asamm,
                nmml_aerial_1999,
                asti_walrus,
                uam_arctos,
                nmml_2000,
                mm_healy_2008,
                mm_healy_2007)

# Isolate occurrences of focal species
all_df_focal <- all_df %>% 
  filter(scientificName %in% c("Odobenus rosmarus", "Phoca largha", "Erignathus barbatus")) %>% 
  
  # Eliminate repeat occurrences
  distinct(all_df_focal) %>% 
  
  # Remove NAs
  na.omit(unique_focal)

# Save this to a csv and shp
st_write(all_df_focal, "../Data_Processed/marine_mammal_integration/marine_mammal_integration_focal_species.csv")

st_write(all_df_focal, "../Data_Processed/marine_mammal_integration/marine_mammal_integration_focal_species.shp")


