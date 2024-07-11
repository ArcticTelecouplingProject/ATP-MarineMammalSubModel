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

# Read all data sets into one integrated CSV -----

# List all CSVs in the TidyScripts folder and read in to one df
tidyscripts <- list.files("../Data_Processed/TidyScripts/",
                          pattern = ".csv$",
                          full.names = TRUE) %>% 
  read_csv() %>% 
  drop_na()

# Convert integrated df to sf object
tidy_sf <- tidyscripts %>%
  st_as_sf(coords = c("decimalLon", "decimalLat"),
           crs = 4326,
           remove = FALSE) %>% 
  st_transform(crs = 3338)


# List individual data sets to the environment -----

# List all files
list.files("../Data_Processed/TidyScripts/", 
           pattern = ".csv$", 
           full.names = TRUE) %>%
  
  # Assign names
  set_names(~ basename(.) %>%
              tools::file_path_sans_ext() %>%
              gsub("^[^_]*_", "", .)) %>%
  
  # Read each CSV and remove any rows that have NAs
  map(~ read_csv(.) %>% 
        drop_na()) %>%
  
  # List individual items to the global environment
  list2env(.GlobalEnv)


# data tidying ------------------------------------------------------------


# Ensure only occurrences of focal species are included
scripts_focal <- tidy_sf %>% 
  filter(scientificName %in% c("Odobenus rosmarus", 
                               "Phoca largha",
                               "Erignathus barbatus")) %>% 
  
  # Eliminate repeat occurrences
  distinct() %>% 
  
  # Remove NAs
  na.omit() %>% 
  
  # Convert necessary columns to numeric
  mutate(year = as.numeric(year),
         month = as.numeric(month),
         day = as.numeric(day),
         count = as.numeric(count),
         decimalLat = as.numeric(decimalLat),
         decimalLon = as.numeric(decimalLon)) 


# Save this to a csv and shp
st_write(scripts_focal, "../Data_Processed/marine_mammal_integration/marine_mammal_integration_focal_species.csv")

st_write(tidy_sf, "../Data_Processed/marine_mammal_integration/marine_mammal_integration_focal_species.shp")


# summaries and plotting --------------------------------------------------

# Optional read-in saved data
scripts_focal <- st_read("../Data_Processed/marine_mammal_integration/marine_mammal_integration_focal_species.shp")

# Set datadir
datadir <- "C:/Users/Natalia Portales/Michigan State University/Waloven, Sydney - ATP-MarineMammal-DataRaw/"
datadir <- "../../ATP-MarineMammal-DataRaw/"

# Summarize by year to see number of locations during each year
all_mams_sum <- scripts_focal %>%
  st_drop_geometry() %>%
  group_by(year) %>%
  summarize(n = n()) %>%
  ungroup() %>% 
  mutate(year_idx = c(1:nrow(.))) # Add time step index
  

all_mams_sum %>% 
  ggplot(aes(x = year, y = n, fill = n)) +
  # ggimage::geom_image(image = img,
  #                     size = 0.03) +
  geom_bar(stat = "identity") +
  labs(
    title = "Marine mammal occurrences in the Bering Strait Region",
    subtitle = "Pacific walrus, spotted seal, bearded seal",
    x = "Year",
    y = "Counts",
    fill = "Counts") +
  scale_fill_viridis_c(direction = -1,
                       na.value = "#dcdcdc") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", size = 20),
        axis.title.x = element_text(face = "bold", size = 14),
        axis.title.y = element_text(face = "bold", size = 14),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        legend.position = "none") # remove legend
  

# Import base map of US/RUS - this is already projected to Alaska Albers 3338
base_map <- st_read(paste0(datadir, "base_map")) %>% 
  # These values came from reducing the extent of the original base_map
  st_crop(xmin = -1500000, ymin = 159766, xmax = 1250000, ymax = 3000000)


# Create tmap by decade
mams_tmap <- scripts_focal %>% 
  mutate(year = as.factor(year),
         
         # Add decade column based on if the year value fell between a certain range
         decade =
           case_when(year %in% c(1970:1979) ~ "1970s",
                     year %in% c(1980:1989) ~ "1980s",
                     year %in% c(1990:1999) ~ "1990s",
                     year %in% c(2000:2009) ~ "2000s",
                     year %in% c(2010:2019) ~ "2010s",
                     year %in% c(2020:2029) ~ "2020s",
                     TRUE ~ NA),
         
         century =
           case_when(year %in% c(1800:1899) ~ "1800s",
                     year %in% c(1900:1999) ~ "1900s",
                     year %in% c(2000:2099) ~ "2000s",
                     TRUE ~ NA),
         
         season =
           case_when(month %in% c(12, 1, 2) ~ "Winter",
                     month %in% c(3:5) ~ "Spring",
                     month %in% c(6:8) ~ "Summer",
                     month %in% c(9:11) ~ "Fall",
                     TRUE ~ NA))

# Facet by century
ggplot() + 
  geom_sf(data = base_map) + 
  geom_sf(data = mams_tmap, mapping = aes(color = factor(season))) +
  facet_wrap(~ century) +
  labs(
    title = "Marine mammal occurrences",
    subtitle = "Bering Strait Region and North Slope, Alaska",
    color = "Season") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", size = 18), # adjust title
        strip.text = element_text(face = "bold", size = 14)) # adjust facet headers


# Plot all bearded seal occurrence points against cropped base map - facet by month
ggplot() + 
  geom_sf(data = base_map) + 
  geom_sf(data = mams_tmap, mapping = aes(color = factor(month))) +
  facet_wrap(~ month,
             
             # Rename the facet labels to their month names instead of the number
             labeller = labeller(month = 
                                   c("1" = "January",
                                     "2" = "February",
                                     "3" = "March",
                                     "4" = "April",
                                     "5" = "May",
                                     "6" = "June",
                                     "7" = "July",
                                     "8" = "August",
                                     "9" = "September",
                                     "10" = "October",
                                     "11" = "November",
                                     "12" = "December"))) +
  labs(
    title = "Marine mammal occurrences",
    subtitle = "Bering Strait Region and North Slope, Alaska",
    color = "Season") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", size = 15), # adjust title
        strip.text = element_text(face = "bold", size = 14), # adjust facet headers
        legend.position = "none") # remove legend
