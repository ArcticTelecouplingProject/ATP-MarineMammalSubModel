# Title: Marine Mammal Data Paper Integration Script
# Author: Sydney Waloven and Natalia Portales
# Date: 2024-11-20
# Description:


# setup -------------------------------------------------------------------

# Load libraries
library(sf)
library(tidyverse)

# Clear environment history 
rm(list = ls())

# Import data -------------------------------------------------------------

## Read options for importing data ##

# Read all data sets downloaded from GBIF into one integrated object -----

# List all CSVs in the TidyScripts folder with 'gbif' in the title 
tidygbif <- list.files("../Data_Processed/TidyScripts/",
                       pattern = "gbif",
                       full.names = TRUE) %>% 
  
  # Assign names
  set_names(~ basename(.) %>% # extracts file name from full file path
              tools::file_path_sans_ext() %>% # removes file extension ('.csv')
              gsub("^[^_]*_", "", .)) %>% # removes all up to and including the first underscore
  
  # Read in to one df
  purrr::map_dfr(~ read_csv(.x),
                 .id = "source_file") %>% # populates source_file col with names from set_names()
  
  # Remove NAs
  drop_na() %>% 
  
  # Convert integrated df to sf object
  st_as_sf(coords = c("decimalLon", "decimalLat"),
           crs = 4326,
           remove = FALSE) %>% 
  st_transform(crs = 3338) # %>% 

# List individual items to the global environment
# list2env(.GlobalEnv)


tidygbif_dedup <- tidygbif %>% 
  
  # Add priority for each row to specify which record to keep when deduplicating. 
  mutate(priority = case_when(
    !str_detect(source, "GBIF") ~ 1, # All non-GBIF sources have the highest priority
    str_detect(source, "GBIF") ~ 2 # GBIF has the lowest priority
  )) %>% 
  
  # Rearrange by priority to keep rows of highest priority
  arrange(scientificName, year, month, day, decimalLat, decimalLon, priority) %>%
  
  # Remove duplicates based on gbifID column
  distinct(gbifID,
           .keep_all = TRUE)
  
# Save data
write_csv(tidygbif_dedup, "../Data_Processed/marine_mammal_integration/tidygbif_dedup.csv")

# Read all data sets NOT downloaded from GBIF into one integrated object -----

# List all CSVs in the TidyScripts folder w/o 'gbif' in the title and read in to one df
tidyother <- list.files("../Data_Processed/TidyScripts/",
                        pattern = ".csv",
                        full.names = TRUE) %>% 
  
  # Exclude all files containing 'gbif' in the name
  .[!grepl("gbif", .)] %>% 
  
  # Assign names
  set_names(~ basename(.) %>% # extracts file name from full file path
              tools::file_path_sans_ext() %>% # removes file extension ('.csv')
              gsub("^[^_]*_", "", .)) %>% # removes all up to and including the first underscore
  
  # Read in to one df
  purrr::map_dfr(~ read_csv(.x),
                 .id = "source_file") %>% # populates source_file col with names from set_names()
  
  # Remove NAs
  drop_na() %>% 
  
  # Convert integrated df to sf object
  st_as_sf(coords = c("decimalLon", "decimalLat"),
           crs = 4326,
           remove = FALSE) %>% 
  st_transform(crs = 3338)


# Read ALL data sets into one integrated object -----

# Since the tidygbif and tidyother objects have a different number of columns (Other is lacking the gbifID column), these will not be able to be merged. tidyother has to get a gbifID column before merging. 

set.seed(108)

# Add gbifID column to tidyother
tidyother <- tidyother %>% 
  
  # Generate values to populate gbifID column
  mutate(gbifID = 1:nrow(.)) %>% 
  
  # Remove duplicates based on exact matches across ALL columns
  distinct(.keep_all = TRUE) %>% 
  
  # Add priority for each row to specify which record to keep when deduplicating. 
  mutate(priority = case_when(
    !str_detect(source, "GBIF") ~ 1, # All non-GBIF sources have the highest priority
    str_detect(source, "GBIF") ~ 2 # GBIF has the lowest priority
  )) %>% 
  
  # Rearrange columns so gbifID is first
  select(gbifID,
         everything())

# Combine tidygbif and tidyother since they have the same number of columns now. 
tidyscripts <- rbind(tidyother, tidygbif_dedup) %>%
  select(gbifID,
         everything()) %>%
  # Eliminate repeat occurrences with duplicate data that may be included from two sources (e.g., a study and a GBIF query)
  distinct(.keep_all = TRUE)

# Save rds
saveRDS(tidyscripts, "../Data_Processed/marine_mammal_integration/tidyscripts_part1.rds")


# data tidying ------------------------------------------------------------

# Optional read-in saved data
tidyscripts <- read_rds("../Data_Processed/marine_mammal_integration/tidyscripts_part1.rds")

# Ensure only occurrences of focal species are included
scripts_focal <- tidyscripts %>% 
  filter(scientificName %in% c("Odobenus rosmarus", 
                               "Phoca largha",
                               "Erignathus barbatus")) %>% 
  
  # Remove NAs
  drop_na(year, month, day, decimalLat, decimalLon) %>% 
  
  # Add Date column
  mutate(
    date = as.Date(paste(year, month, day, sep = "-"))
  ) %>% 
  
  # Reorder columns
  select(gbifID,
         scientificName,
         date,
         decimalLat,
         decimalLon,
         count,
         methodObs,
         source,
         dataOrigin,
         source_file)

# Save this to a csv and rds
write_csv(scripts_focal, "../Data_Processed/marine_mammal_integration/final_marine_mammal_integration.csv", append = FALSE)

write_rds(scripts_focal, "../Data_Processed/marine_mammal_integration/final_marine_mammal_integration.rds")
# This doesn't abbreviate column names like when saving as a shapefile and still maintains the sf class.

# Save as shp
st_write(scripts_focal, "../Data_Processed/marine_mammal_integration/final_marine_mammal_integration.shp", append = FALSE)


# summaries and plotting --------------------------------------------------

# Optional read-in saved data
scripts_focal <- read_rds("../Data_Processed/marine_mammal_integration/final_marine_mammal_integration.rds") # maintains sf df and date col as POSIX/date

scripts_focal <- read_csv("../Data_Processed/marine_mammal_integration/final_marine_mammal_integration.csv") %>% 
  st_as_sf(coords = c("decimalLon", "decimalLat"),
           crs = 4326,
           remove = FALSE) %>% 
  st_transform(crs = 3338) %>% 
  mutate(date = as.Date(date))

# Set datadir
datadir <- "C:/Users/Natalia Portales/Michigan State University/Waloven, Sydney - ATP-MarineMammal-DataRaw/"
datadir <- "../../ATP-MarineMammal-DataRaw/"

# Import base map of US/RUS - this is already projected to Alaska Albers 3338
base_map <- st_read(paste0(datadir, "base_map")) %>% 
  st_crop(xmin = -2200000, ymin = 160000, xmax = 1250000, ymax = 3000000)

# Visualize base map
# ggplot(data = base_map) +
#   geom_sf() 

# Summarize by year to see number of locations during each year
all_mams_sum <- scripts_focal %>%
  st_drop_geometry() %>%
  group_by(year(date)) %>%
  summarize(n = n()) %>%
  ungroup() %>% 
  mutate(year_idx = c(1:nrow(.))) # Add time step index


# Bar plot of occurrences by year -----
mams_sum <- all_mams_sum %>% 
  ggplot(aes(x = `year(date)`, y = n, fill = n)) +
  # ggimage::geom_image(image = img,
  #                     size = 0.03) +
  geom_bar(stat = "identity") +
  labs(
    # title = "Marine mammal occurrences in the Bering Strait Region",
    # subtitle = "Pacific walrus, spotted seal, bearded seal",
    x = "Year",
    y = "Occurrences",
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

# Save plot
ggsave("../Figures/final/all_mams_sum_bsr.png", plot = mams_sum, width = 10, height = 8)


# Create plot by decade
mams_tmap <- scripts_focal %>% 
  mutate(year = year(date),
         month = month(date),
         
         # Add decade column based on if the year value fell between a certain range
         decade = case_when(
           year %in% 1970:1979 ~ "1970s",
           year %in% 1980:1989 ~ "1980s",
           year %in% 1990:1999 ~ "1990s",
           year %in% 2000:2009 ~ "2000s",
           year %in% 2010:2019 ~ "2010s",
           year %in% 2020:2029 ~ "2020s",
           TRUE ~ NA_character_
         ),
         
         century = case_when(
           year %in% 1800:1899 ~ "1800s",
           year %in% 1900:1999 ~ "1900s",
           year %in% 2000:2099 ~ "2000s",
           TRUE ~ NA_character_
         ),
         
         season = case_when(
           month %in% c(12, 1, 2) ~ "Winter",
           month %in% 3:5 ~ "Spring",
           month %in% 6:8 ~ "Summer",
           month %in% 9:11 ~ "Fall",
           TRUE ~ NA_character_
         )
  )


# Plot occurrences on base map - facet by century -----
p1 <- ggplot() + 
  geom_sf(data = base_map) + 
  geom_sf(data = mams_tmap, mapping = aes(color = factor(season))) +
  facet_wrap(~ century) +
  labs(
    # title = "Marine mammal occurrences from 1860-2024",
    # subtitle = "Bering Strait Region and North Slope, Alaska",
    color = "Season") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", size = 18), # adjust title
        strip.text = element_text(face = "bold", size = 14)) # adjust facet headers

# Save plot
ggsave("../Figures/final/marine_mams_facet_century.png", plot = p1, width = 10, height = 8)


# Plot occurrences on base map - facet by month -----
p2 <- ggplot() + 
  geom_sf(data = base_map) + 
  geom_sf(data = mams_tmap, mapping = aes(color = factor(month),
                                          alpha = 0.7)) +
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
    # title = "Marine mammal occurrences from 1860-2024",
    # subtitle = "Bering Strait Region and North Slope, Alaska",
    color = "Season") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", size = 15), # adjust title
        strip.text = element_text(face = "bold", size = 14), # adjust facet headers
        legend.position = "none") # remove legend

# Save plot
ggsave("../Figures/final/marine_mams_facet_month.png", plot = p2, width = 10, height = 8)


# Plot occurrences by species -----
p3 <- ggplot() + 
  geom_sf(data = base_map) + 
  geom_sf(data = mams_tmap, mapping = aes(color = factor(season),
                                          alpha = 0.7),
          show.legend = c(alpha = FALSE)) +
  facet_wrap(~ scientificName) +
  labs(
    # title = "Marine mammal occurrences from 1860-2024",
    # subtitle = "Bering Strait Region and North Slope, Alaska",
    color = "Season") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", size = 15), # adjust title
        strip.text = element_text(face = "bold", size = 12))

# Save plot
ggsave("../Figures/final/marine_mams_facet_species.png", plot = p3, width = 10, height = 8)


# Create timeline of collection years for each data source -----
p4 <- scripts_focal %>% 
  mutate(
    # Add column to put data sources in order
    table_num = as.numeric(
      case_when(
        str_detect(source_file, "queries_walrus") ~ "1",
        str_detect(source_file, "queries_spotted") ~ "2",
        str_detect(source_file, "queries_bearded") ~ "3",
        str_detect(source_file, "tgs_seismic") ~ "4",
        str_detect(source_file, "norseman_2015") ~ "5",
        str_detect(source_file, "mmw_2016") ~ "6",
        str_detect(source_file, "mmw_2017") ~ "7",
        str_detect(source_file, "laurier_2019") ~ "8",
        str_detect(source_file, "laurier_2018") ~ "9",
        str_detect(source_file, "healy_2018") ~ "10",
        str_detect(source_file, "nmml_aerial") ~ "11",
        str_detect(source_file, "sex_age") ~ "12",
        str_detect(source_file, "behavior_uas") ~ "13",
        str_detect(source_file, "moudry") ~ "14",
        str_detect(source_file, "asamm") ~ "15",
        str_detect(source_file, "cetacean_survey") ~ "16",
        str_detect(source_file, "asti") ~ "17",
        str_detect(source_file, "arctos") ~ "18",
        str_detect(source_file, "bering_survey") ~ "19",
        str_detect(source_file, "beringia_2008") ~ "20",
        str_detect(source_file, "beringia_2007") ~ "21",
        str_detect(source_file, "lamont") ~ "22",
        str_detect(source_file, "acoustic_mams") ~ "23",
        str_detect(source_file, "norseman_2017") ~ "24",
        TRUE ~ NA_character_)
    )
  ) %>% 
  st_drop_geometry() %>% 
  group_by(table_num, source) %>%
  summarize(
    # Create collection periods with start and end dates for each data source
    start_date = min(date),
    end_date = max(date),
    length_years = as.numeric(difftime(max(date), min(date), units = "days")) / 365.25,
    length_days = as.numeric(difftime(max(date), min(date), units = "days")),
    
    # Drop grouping
    .groups = "drop"
  ) %>%
  
  # Arrange numerically
  arrange(table_num) %>% 
  
  # Set factor levels for "source" based on numerical order, and reverse
  mutate(source = factor(source, levels = source[order(table_num)]),
         source = forcats::fct_rev(source)) %>% 
  
  # Plot collection periods
  ggplot(aes(x = start_date, y = source, xend = end_date, yend = source)) +
  geom_segment(linewidth = 3, color = "lightsteelblue2") +
  geom_text(aes(label = paste0(format(start_date, "%Y"), " -")),
            vjust = -0.5,
            hjust = 1, 
            size = 3,
            color = "black") +
  geom_text(aes(x = end_date, label = paste0(" ", format(end_date, "%Y"))), 
            vjust = -0.5, 
            hjust = 0,
            size = 3, 
            color = "black") +
  labs(# title = "Collection Period by Data Source",
    x = "Year",
    y = "Dataset") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", size = 20),
        axis.title.x = element_text(face = "bold", size = 17),
        axis.title.y = element_text(face = "bold", size = 17),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 12))

# Save plot
ggsave("../Figures/final/collection_periods_ordered.png", plot = p4, width = 18, height = 8)


# Create bar plot of number of observations per data source -----
p5 <- scripts_focal %>% 
  mutate(
    # Add column to put data sources in order
    table_num = as.numeric(
      case_when(
        str_detect(source_file, "queries_walrus") ~ "1",
        str_detect(source_file, "queries_spotted") ~ "2",
        str_detect(source_file, "queries_bearded") ~ "3",
        str_detect(source_file, "tgs_seismic") ~ "4",
        str_detect(source_file, "norseman_2015") ~ "5",
        str_detect(source_file, "mmw_2016") ~ "6",
        str_detect(source_file, "mmw_2017") ~ "7",
        str_detect(source_file, "laurier_2019") ~ "8",
        str_detect(source_file, "laurier_2018") ~ "9",
        str_detect(source_file, "healy_2018") ~ "10",
        str_detect(source_file, "nmml_aerial") ~ "11",
        str_detect(source_file, "sex_age") ~ "12",
        str_detect(source_file, "behavior_uas") ~ "13",
        str_detect(source_file, "moudry") ~ "14",
        str_detect(source_file, "asamm") ~ "15",
        str_detect(source_file, "cetacean_survey") ~ "16",
        str_detect(source_file, "asti") ~ "17",
        str_detect(source_file, "arctos") ~ "18",
        str_detect(source_file, "bering_survey") ~ "19",
        str_detect(source_file, "beringia_2008") ~ "20",
        str_detect(source_file, "beringia_2007") ~ "21",
        str_detect(source_file, "lamont") ~ "22",
        str_detect(source_file, "acoustic_mams") ~ "23",
        str_detect(source_file, "norseman_2017") ~ "24",
        TRUE ~ NA_character_)
    )
  ) %>% 
  st_drop_geometry() %>% 
  group_by(table_num, source, scientificName) %>%
  summarize(
    # Sum total number of occurrences for each data source
    num_obs = n(),
    
    # Drop grouping
    .groups = "drop"
  ) %>% 
  
  # Arrange numerically
  arrange(table_num) %>% 
  
  # Set factor levels for "source" based on numerical order, and reverse
  mutate(source = factor(source, levels = source[order(table_num)]),
         source = forcats::fct_rev(source)) %>% 
  
  # Plot occurrences by data source
  ggplot(aes(x = num_obs, y = source, fill = scientificName)) +
  geom_bar(stat = "identity") +
  # geom_text(aes(label = num_obs),
  #           position = position_stack(vjust = 0.5), # centers text within the bar
  #           color = "black",
  #           size = 4) +
  labs(# title = "Marine Mammal Aggregated Count by Data Source",
       x = "Occurrences",
       y = "Data Source",
       fill = "Scientific Name") +
  scale_fill_manual(values = c("slategray4", "burlywood4", "lightblue2")) +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", size = 20),
        axis.title.x = element_text(face = "bold", size = 17),
        axis.title.y = element_text(face = "bold", size = 17),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.title = element_text(face = "bold"))

# Save plot
ggsave("../Figures/final/occurrences_by_source.png", plot = p5, width = 12, height = 8)







