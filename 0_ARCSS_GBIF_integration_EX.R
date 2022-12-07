# Merging walrus from NSF_ARCSS_GBIF data with TGS seismic survey 2013 data
# 26Oct2022

# This script is the overall code to clean, transform, and edit data for the merging of
# data sets included here (NSF ARCSS GBIF walrus data and TGS seismic survey 2013 data).
# This script can be used as a baseline example for tidying subsequent AMM datasets
# (e.g., bearded seal and spotted seal datasets in the NSF ARCSS drive from GBIF). 


# Libraries and Packages --------------------------------------------------

library(terra)
library(sf)
library(tidyverse)

# Clear history 
rm(list=ls())

# Pacific walrus data set #1 ----------------------------------------------

# Import raw walrus occurrence data
walrus_gbif <- st_read("NSF_ARCSS_GBIF/Data_Raw/odobenus_rosmarus_divergens")

head(walrus_gbif)

# Transform to Alaska Albers projection
walrus_gbif_3338 <- walrus_gbif %>% st_transform(crs = 3338)

## OR ##
# Read in Alaska Albers projected full dataset
# walrus3338 <- st_read("NSF_ARCSS_GBIF/Data_Processed/WalrusData_3338.shp")
## ## ## 

# Check to see if it's properly projected (NAD83 / Alaska Albers)
st_crs(walrus_gbif_3338)


# Clean up this dataset further

## Get rid of unneeded columns 
walrus_gbif_new <- walrus_gbif_3338[ , 14:50]

## Get rid of unnecessary NA values (if any)
walrus_gbif_new <- walrus_gbif_new[!is.na(walrus_gbif_new$decimalLat), ]

# Remove extraneous columns - use select function to choose which columns to keep in new df
colnames(walrus_gbif_new)
wal_gbif_new <- walrus_gbif_new %>% dplyr::select(genericNam,
                                                  specificEp,
                                                  infraspeci,
                                                  decimalLat,
                                                  decimalLon,
                                                  year,
                                                  month,
                                                  day)
## Geometry column will automatically be included too because of the spatial structure
## Can get rid of with st_drop_geometry()

# Add a new column to specify data source to backtrack later
wal_gbif_new$source <- "ARCSS GBIF Pacific Walrus"

# How is R classifying each column of data?
lapply(wal_gbif_new, class)

# Convert numeric columns to numeric - probably a more succinct way to do this but that's okay
wal_gbif_new$decimalLat <- as.numeric(wal_gbif_new$decimalLat)
wal_gbif_new$decimalLon <- as.numeric(wal_gbif_new$decimalLon)
wal_gbif_new$year <- as.numeric(walrus_gbif_new$year)
wal_gbif_new$month <- as.numeric(walrus_gbif_new$month)
wal_gbif_new$day <- as.numeric(wal_gbif_new$day)

# Combine species and subspecies columns into one col and make one character string
wal_gbif_new$genericNam <- gsub("\"", "", wal_gbif_new$genericNam)
wal_gbif_new$specificEp <- gsub("\"", "", wal_gbif_new$specificEp)
wal_gbif_new$infraspeci <- gsub("\"", "", wal_gbif_new$infraspeci)
# Add that character string to a new column
wal_gbif_new$scientificName <- paste(wal_gbif_new$genericNam, wal_gbif_new$specificEp, wal_gbif_new$infraspeci)

# Move scientificName column to first column
walrus_gbif_clean <- wal_gbif_new %>% select(scientificName, everything())

# Remove old columns of genus, species, and subspecies
walrus_gbif_clean <- walrus_gbif_clean %>% dplyr::select(scientificName,
                                                         year,
                                                         month,
                                                         day,
                                                         decimalLat,
                                                         decimalLon,
                                                         source,
                                                         geometry)

# Add a common name column to the walrus_gbif_clean df and fill it with 'Walrus'
walrus_gbif_clean$commonName <- "Walrus"

# Rearrange column names to match dfs - or I think this can be solved with whatever merge function ends up used
walrus_gbif_merge <- walrus_gbif_clean %>% dplyr::select(scientificName,
                                                         commonName,
                                                         year,
                                                         month,
                                                         day,
                                                         decimalLat,
                                                         decimalLon,
                                                         source,
                                                         geometry)

# Missing a groupSize column in the walrus_gbif_merge df??
# Add groupSize column to walrus_gbif_merge df based on whether there are no NAs in the year, month, day cols
## Reference for code:
## https://www.geeksforgeeks.org/how-to-add-a-column-based-on-other-columns-in-r-dataframe/
walrus_gbif_merge <- transform(walrus_gbif_clean, groupSize = ifelse
                               (is.na(year) & is.na(month) & is.na(day), 0, 1))
# Right now this is marking records with no YMD as 0
# Decide whether to mark records with an NA in any part of the YMD as 0?

# Rearrange column names to match dfs - or I think this can be solved with whatever merge function ends up used
walrus_gbif_merge <- walrus_gbif_merge %>% dplyr::select(scientificName,
                                                         commonName,
                                                         year,
                                                         month,
                                                         day,
                                                         groupSize,
                                                         decimalLat,
                                                         decimalLon,
                                                         source,
                                                         geometry)


## Tada now you have a clean and tidy dataset to work with and merge

View(walrus_gbif_merge)

# Save this df as a csv for importing - need to use st_write because of spatial geometry
# st_write(walrus_gbif_merge, "ATP_Data_Integration/Data_Processed/1-1_walrus_arcss_gbif.csv")


# TGS Seismic Survey Chukchi 2013 data set #2 -----------------------------

# Read in TGS Seismic data from DataIntegration_Tutorial
seismic_obis <- read.csv("ATP_Data_Integration/Data_Raw/obis_seamap_dataset_1110_csv_71491/obis_seamap_dataset_1110_points.csv")

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
# seismic_new2$group_size <- as.integer(seismic_new2$group_size)

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

## Woo now you have a second tidy data set to work with and merge!

View(seismic_new2)

# Save this df to a csv for integration importing
# st_write(seismic_new2, "ATP_Data_Integration/Data_Processed/2_tgs_seismic_2013.csv")



# Merge data sets ---------------------------------------------------------
# Need to make sure that everything is standardized like the columns, column names, and format
# of the columns

# Standardize column names
colnames(walrus_gbif_merge)
colnames(seismic_new2)

# Subset out just the walrus data from the seismic df
seismic_new2 <- subset(seismic_new2, common_name.x == "Walrus")

# Renaming column names from the seismic_new2 df to match the walrus_gbif_clean df
seismic_merge <- seismic_new2 %>% rename(scientificName = scientific_name,
                                         commonName = common_name.x,
                                         groupSize = group_size,
                                         decimalLat = latitude,
                                         decimalLon = longitude)
View(seismic_merge)


# Merge just the walrus data from both dfs
all_df <- rbind(walrus_gbif_merge, seismic_merge)


# Summary Stats -----------------------------------------------------------


# create new df with occurrence points and no geometry attached
all_df_drop <- all_df %>%
  st_drop_geometry() %>%
  group_by(year, month, day) %>%
  summarize(n=n()) %>%
  ungroup()

# check classes to make sure year and month cols are numeric
all_df_drop$year <- as.numeric(all_df_drop$year)
all_df_drop$month <- as.numeric(all_df_drop$month) 

class(all_df_drop$year)
class(all_df_drop$month)

## For loop to avoid rewriting code each time to look at the sum of pts since a year

# number of months per year
n.months <- n_distinct(all_df_drop$month)
# unique years in data set, in order from earliest to latest
my.years <- sort(unique(all_df_drop$year))
# number of years
# n.years <- n_distinct(all_df_drop$year) ## with NA
n.years <- n_distinct(my.years)
# my.sums will contain each of the cumulative sums across the years
my.sums <- rep(NA, n.years)

## choose one of the time windows for the loop
month_selected <- seq(1,12);
time_window = 'year round' ## year round

month_selected <- seq(6,10);
time_window = 'summer' ## summer

month_selected <- c(11, 12, seq(1,5));
time_window = 'winter' ## winter

sum_since <- data.frame()
for (i in 1:n.years) {
  my.sums[i] <- all_df_drop %>%
    dplyr::filter(year >= my.years[i]) %>%
    dplyr::filter(month %in% month_selected) %>%
    dplyr::select(n) %>%
    sum(na.rm = TRUE)
  
  # print out the result for easy checking
  cat(my.years[i], ':', my.sums[i], '\n')
  
  # put the results into a dataframe
  sum_i <- cbind(year = my.years[i], sum = my.sums[i], time_window = time_window)
  sum_since <- rbind(sum_since, sum_i)
}

# st_write(sum_since, "ATP_Data_Integration/Data_Processed/2_ARCSS_GBIF_integration_winter.csv")



# summarize every column in data frame
summary(all_df_drop[c('year', 'month', 'day', 'n')])                      
