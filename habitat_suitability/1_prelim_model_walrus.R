# Sydney Waloven
# 20Aug2023
# This script was adapted from my final project for IBIO870 Spatial Ecology and
# updated from my final project for IBIO890 Statistical Models. It also takes into
# account the edit suggestions from Anh Do from CSTAT. 
# The purpose of this script is to create preliminary habitat suitability models.



# Loading preliminaries ---------------------------------------------------

library(sf)
library(tidyverse)
library(dismo)
library(raster)
library(plyr)
library(terra)
library(gstat)
library(maptools)

# Clear history
rm(list = ls())

# Importing and cleaning data ---------------------------------------------

datadir <- "../Data_Processed/"

# Import integrated walrus data
all_walrus <- st_read(paste0(datadir, "odobenus_integrations/odobenus_integrated20230821.shp"))

# Rename columns
all_walrus <- all_walrus %>% dplyr::rename(
  scientificName = scntfcN,
  commonName = commnNm,
  groupSize = groupSz,
  decimalLat = decmlLt,
  decimalLon = decmlLn,
  methodObservation = mthdObs)

# Create vector of columns that should be numeric
char_cols <- c("decimalLat", "decimalLon", "year", "month", "day", "groupSize")

# Convert from character to numeric across all columns using dplyr
all_walrus <- all_walrus %>% mutate(across(all_of(char_cols)))

# Check column classes
lapply(all_walrus, class)


# Separate occurrences by season ------------------------------------------

# Add new column to all_walrus that says which season the occurrence was in
# If the month column contains values 6, 7, 8 then the season col fills with "summer"
all_walrus$season <- ifelse(all_walrus$month %in% c(6, 7, 8), "summer",
                            # If it does not fall in summer, then it checks to see if the col contains 
                            # 9, 10, 11 and if so then the season col fills with "fall".
                            # If it falls outside both of these set ranges then it fills with true NA.
                            ifelse(all_walrus$month %in% c(9, 10, 11), "fall", NA))

# Summer occurrences from 2015-2022
summer <- all_walrus %>% dplyr::filter(season == "summer", year > 2014)
# Fall occurrences from 2015-2022
fall <- all_walrus %>% dplyr::filter(season == "fall", year > 2014)


occurrence_n <- all_walrus %>%
  dplyr::count(year, season, source) %>%
  filter(!is.na(season)) %>%
  arrange(year, season, source) %>%
  st_cast("POINT") # Why is this grouping points into multipoints?


# Environmental Data ------------------------------------------------------

# Create the folders (directories) "data"
# if they exist already, this command won't over-write them.
# create directory called "data"

datadir <- "../../ATP-MarineMammal-DataRaw/"

# Load the data and subset by season --------------------------------------
# This loads the environmental variables found in the Ascii_Environmental_Variables
# folder in Data_Raw. This section creates "sum" and "fall" variables for each
# predictor to create seasonal and annual RasterStacks in the next section.

# Chlorophyll Concentration
chloro_conc <- list.files(paste0(datadir, "Ascii_Environmental_Variables/Chlorophyll_Concentration"),
                          pattern = "*.asc",
                          full.names = TRUE)

# Select only the mean files - excluding the standard deviation files - for each season
# aggregated for all years
# Summer: june-aug
chloro_ja <- chloro_conc[grepl(pattern = "ja", x = chloro_conc)] # takes all the files with "ja"
# Fall: sep-nov
chloro_sn <- chloro_conc[grepl(pattern = "sn", x = chloro_conc)] # takes all the files with "sn"

chloro_sum <- chloro_ja[grepl(pattern = "_mn", x = chloro_ja)] # from ja, takes all the files with "_mn"
chloro_fall <- chloro_sn[grepl(pattern = "_mn", x = chloro_sn)] # from sn, takes all the files with "_mn"

# raster(chloro_conc[1]) # check value

# Distance to Land
dist_land <- list.files(paste0(datadir, "Ascii_Environmental_Variables/Distance_to_Land"),
                        pattern = "*.asc",
                        full.names = TRUE)
## This is just one file


# Sea Ice Extent
sea_ice <- list.files(paste0(datadir, "Ascii_Environmental_Variables/Sea_Ice_Extent"),
                      pattern = "*.asc",
                      full.names = TRUE)

# Select files for each season - aggregated for all years
# Summer: june-aug
ice_sum <- sea_ice[grepl(pattern = "ja", x = sea_ice)] # takes all the files with "ja"
# Fall: sep-nov
ice_fall <- sea_ice[grepl(pattern = "sn", x = sea_ice)] # takes all the files with "sn"


# Sea Surface Temperature
sst <- list.files(paste0(datadir, "Ascii_Environmental_Variables/Sea_Surface_Temperature"),
                  pattern = "*.asc",
                  full.names = TRUE)

# Select only the mean files - excluding the standard deviation files - for each season
# aggregated for all years
# Summer: june-aug
sst_ja <- sst[grepl(pattern = "ja", x = sst)] # takes all the files with "ja"
# Fall: sep-nov
sst_sn <- sst[grepl(pattern = "sn", x = sst)] # takes all the files with "sn"

sst_sum <- sst_ja[grepl(pattern = "_mn", x = sst_ja)] # from ja, takes all the files with "_mn"
sst_fall <- sst_sn[grepl(pattern = "_mn", x = sst_sn)] # from sn, takes all the files with "_mn"


# Shipping
# Data is aggregated for the years 2015-2020
shipping <- list.files(paste0(datadir, "Ascii_Environmental_Variables/shipping"),
                       pattern = "*.asc",
                       full.names = TRUE)

# Select files for each season - aggregated for all years
# Summer: june-aug
ship_sum <- shipping[grepl(pattern = "summer", x = shipping)] # takes all the files with "ja"
# Fall: sep-nov
ship_fall <- shipping[grepl(pattern = "fall", x = shipping)] # takes all the files with "sn"


## Now, take observations from 2015-2020 and create 1 stack per year, only extracting
## observations from that year for that stack. 

# Raster Stacks -----------------------------------------------------------
# Iterate over the numbers 15-20 to create a raster stack for each predictor and year
# for chlorophyll concentration, sea ice, and sst. When extracting observations from
# one year, you want environmental values that are dynamically being pulled from the 
# same year.
# E.g., 2015 observations get 2015 values, etc. 

## Chlorophyll Concentration
for (num in 15:20) {
  # Construct the pattern to search for files
  pattern1 <- paste0(num, "_ja_mn") # only want the mean files
  pattern2 <- paste0(num, "_sn_mn") 
  
  # Check if the pattern exists in the file names
  if (any(grepl(pattern1, chloro_sum))) {
    # Summer
    chloro <- chloro_sum[grepl(pattern = pattern1, x = chloro_sum)]
    chloro_stack <- stack(chloro) # Create chloro stack with files containing pattern
    assign(paste0("chloro_sum_", num), chloro_stack) # Rename files for summer
  }
  # Fall
  if (any(grepl(pattern2, chloro_fall))) {
    chloro <- chloro_fall[grepl(pattern = pattern2, x = chloro_fall)]
    chloro_stack <- stack(chloro) # Create chloro stack with files containing pattern
    assign(paste0("chloro_fall_", num), chloro_stack) # Rename files for fall
  }
}


## Sea Ice Extent
for (num in 15:20) {
  # Construct the pattern to search for files
  # sum (sm) - corresponds to the number of days when ice was present in each pixel
  pattern1 <- paste0(num, "_ja_sm") 
  pattern2 <- paste0(num, "_sn_sm") 
  
  # Check if the pattern exists in the file names
  if (any(grepl(pattern1, ice_sum))) {
    # Summer
    ice <- ice_sum[grepl(pattern = pattern1, x = ice_sum)]
    ice_stack <- stack(ice) # Create chloro stack with files containing pattern
    assign(paste0("ice_sum_", num), ice_stack) # Rename files for summer
  }
  # Fall
  if (any(grepl(pattern2, ice_fall))) {
    ice <- ice_fall[grepl(pattern = pattern2, x = ice_fall)]
    ice_stack <- stack(ice) # Create chloro stack with files containing pattern
    assign(paste0("ice_fall_", num), ice_stack) # Rename files for fall
  }
}


## Sea Surface Temperature
for (num in 15:20) {
  # Construct the pattern to search for files
  pattern1 <- paste0(num, "_ja_mn") # only want the mean files
  pattern2 <- paste0(num, "_sn_mn") 
  
  # Check if the pattern exists in the file names
  if (any(grepl(pattern1, sst_sum))) {
    # Summer
    sst <- sst_sum[grepl(pattern = pattern1, x = sst_sum)]
    sst_stack <- stack(sst) # Create chloro stack with files containing pattern
    assign(paste0("sst_sum_", num), sst_stack) # Rename files for summer
  }
  # Fall
  if (any(grepl(pattern2, sst_fall))) {
    sst <- sst_fall[grepl(pattern = pattern2, x = sst_fall)]
    sst_stack <- stack(sst) # Create chloro stack with files containing pattern
    assign(paste0("sst_fall_", num), sst_stack) # Rename files for fall
  }
}

# Outputs should be a raster stack for each season of each predictor (2 per predictor)
# for each year between 2015-2020
# e.g., chloro_sum_15, chloro_fall_15, ice_sum_15, ice_fall_15,
# sst_sum_15, sst_fall_15

# Distance to Land
dist_stack <- stack(dist_land) # creates one stack for dist_land that can be used for
# either fall or summer

# Shipping
ship_sum_stack <- stack(ship_sum) # creates summer stack for shipping
ship_fall_stack <- stack(ship_fall) # creates fall stack for shipping

# Now you have all the ascii files as RasterStacks for each year through 2015-2020.

# Plot to check
chloro_sum_15 %>% plot()
chloro_fall_15 %>% plot()
ice_sum_15 %>% plot()
ice_fall_15 %>% plot()
sst_sum_15 %>% plot()
sst_fall_15 %>% plot()
dist_stack %>% plot()
ship_sum_stack %>% plot()
ship_fall_stack %>% plot()


# Combining raster stacks -------------------------------------------------
# Combine all predictors (chloro, ice, sst, dist, ship) into one stack per year
# per season and set to same crs of occurrence data

# Save crs of all_walrus
new_crs <- crs(all_walrus)

# Create RasterStack of all predictors for summer - highlight and run whole loop at once
# or it may throw an error 
for (year in 15:20) {
  chloro_sum <- get(paste0("chloro_sum_", year))
  ice_sum <- get(paste0("ice_sum_", year))
  sst_sum <- get(paste0("sst_sum_", year))
  
  year_stack <- stack(c(chloro_sum, ice_sum, sst_sum, dist_stack, ship_sum_stack))
  # Set crs to same crs of occurrence data
  year_stack <- projectRaster(year_stack, crs = new_crs)
  
  assign(paste0("pred_sum", year), year_stack)
}

# Create RasterStack of all predictors for fall - highlight and run whole loop at once
# or it may throw an error 
for (year in 15:20) {
  chloro_fall <- get(paste0("chloro_fall_", year))
  ice_fall <- get(paste0("ice_fall_", year))
  sst_fall <- get(paste0("sst_fall_", year))
  
  year_stack <- stack(c(chloro_fall, ice_fall, sst_fall, dist_stack, ship_fall_stack))
  # Set crs to same crs of occurrence data
  year_stack <- projectRaster(year_stack, crs = new_crs)
  
  assign(paste0("pred_fall", year), year_stack)
}

# The outputs of these two for loops will be rasterBricks which are a collection of
# raster layers that represent the same variable at different points in time. 


# Extract values from predictor rasters -----------------------------------
# So now you need to get predictor values at the locations in time when there are
# occurrences for each year and season

for (num in 2015:2022) { 
  # Construct the pattern to loop over predictor values
  pattern1 <- paste0("pred_sum", num)
  pattern2 <- paste0("pred_fall", num) 
  
  # Check if the pattern exists in the file names
  if (any(grepl(pattern1, names(.GlobalEnv)))) {
    # Summer
    presvals <- raster::extract(get(paste0("pred_sum", num)), get(paste0("walrus", num)))
    assign(paste0("presvals_sum", num), presvals) # Rename files for summer
  }
  # Fall
  if (any(grepl(pattern2, names(.GlobalEnv)))) {
    presvals <- raster::extract(get(paste0("pred_fall", num)), get(paste0("walrus", num)))
    assign(paste0("presvals_fall", num), presvals) # Rename files for fall
  }
}

# Setting random seed to always create the same random set of points for this example
set.seed(0)

# Generate background points
# Loop through years 2015-2019 since we are only looking at years when there are occurrences
for (num in 2015:2022) {
  # Construct the pattern to loop over predictor values
  pattern1 <- paste0("pred_sum", num)
  pattern2 <- paste0("pred_fall", num) 
  
  if (any(grepl(pattern1, names(.GlobalEnv)))) {
    # Get the number of occurrences for the current year
    num_occ <- nrow(get(paste0("walrus", num)))
    # Chose number of random points based on number of species occurrences for that year
    backgr_sum_wal <- randomPoints(get(paste0("pred_sum", num)), num_occ)
    assign(paste0("backgr_sum_wal", num), backgr_sum_wal)
  }
  if (any(grepl(pattern2, names(.GlobalEnv)))) {
    # Get the number of occurrences for the current year
    num_occ <- nrow(get(paste0("walrus", num)))
    # Changed # of random points based on number of species occurrences for that year
    backgr_fall_wal <- randomPoints(get(paste0("pred_fall", num)), num_occ)
    assign(paste0("backgr_fall_wal", num), backgr_fall_wal)
  }
}

# The background points are generated dynamically based on the number of walrus
# occurrences for each year.

# Generate absent values based on the background points
for (num in 2015:2019) {
  # Get the current year and make sure it's numeric
  year <- as.numeric(num)
  
  # Create vector for seasons
  seasons <- c("summer", "fall")
  
  for (season in seasons) {
    # Construct the pattern to loop over predictor values and background points
    pattern_sum <- paste0("backgr_sum_wal", num)
    pattern_fall <- paste0("backgr_fall_wal", num)
    
    # Check if there are any background points for summer or fall
    if (season == "summer" & any(grepl(pattern_sum, names(.GlobalEnv)))) {
      absvals_sum_wal <- raster::extract(get(paste0("pred_sum", num)), get(paste0("backgr_sum_wal", num)))
      assign(paste0("absvals_sum_wal", num), absvals_sum_wal)
    } else if (season == "fall" & any(grepl(pattern_fall, names(.GlobalEnv)))) {
      absvals_fall_wal <- raster::extract(get(paste0("pred_fall", num)), get(paste0("backgr_fall_wal", num)))
      assign(paste0("absvals_fall_wal", num), absvals_fall_wal)
    }
  }
}


# Generate tables for presence/background points --------------------------
# pb indicates whether this is a presence or background point

# summer
pb_sum15 <- c(rep(1, nrow(presvals_sum15)), rep(0, nrow(absvals_sum_wal15)))
pb_sum16 <- c(rep(1, nrow(presvals_sum16)), rep(0, nrow(absvals_sum_wal16)))
pb_sum17 <- c(rep(1, nrow(presvals_sum17)), rep(0, nrow(absvals_sum_wal17)))
pb_sum18 <- c(rep(1, nrow(presvals_sum18)), rep(0, nrow(absvals_sum_wal18)))
pb_sum19 <- c(rep(1, nrow(presvals_sum19)), rep(0, nrow(absvals_sum_wal19)))

# fall
pb_fall15 <- c(rep(1, nrow(presvals_fall15)), rep(0, nrow(absvals_fall_wal15)))
pb_fall16 <- c(rep(1, nrow(presvals_fall16)), rep(0, nrow(absvals_fall_wal16)))
pb_fall17 <- c(rep(1, nrow(presvals_fall17)), rep(0, nrow(absvals_fall_wal17)))
pb_fall18 <- c(rep(1, nrow(presvals_fall18)), rep(0, nrow(absvals_fall_wal18)))
pb_fall19 <- c(rep(1, nrow(presvals_fall19)), rep(0, nrow(absvals_fall_wal19)))




# Create data frames ------------------------------------------------------


# summer data frame
sdmdata_sum_wal <- data.frame(cbind(pb_sum15, pb_sum16, pb_sum17, pb_sum18, pb_sum19,
                                    rbind(presvals_sum15, presvals_sum16, presvals_sum17,
                                          presvals_sum18, presvals_sum19, absvals_sum_wal15,
                                          absvals_sum_wal16, absvals_sum_wal17, absvals_sum_wal18,
                                          absvals_sum_wal19)))
# fall data frame
sdmdata_fall_wal <- data.frame(cbind(pb_fall15, pb_fall16, pb_fall17, pb_fall18, pb_fall19,
                                     rbind(presvals_fall15, presvals_fall16, presvals_fall17,
                                           presvals_fall18, presvals_fall19, absvals_fall_wal15,
                                           absvals_fall_wal16, absvals_fall_wal17, absvals_fall_wal18,
                                           absvals_fall_wal19)))

head(sdmdata_sum_wal)
head(sdmdata_fall_wal)


# Save work ---------------------------------------------------------------

saveRDS(sdmdata_sum_wal, "../Data_Processed/prelim_models/CSTAT/sdmdata_sum_wal.Rds") # summer
saveRDS(sdmdata_fall_wal, "../Data_Processed/prelim_models/CSTAT/sdmdata_fall_wal.Rds") # fall

# predictor values at occurrence locations
saveRDS(presvals_sum15, "../Data_Processed/prelim_models/CSTAT/pvals_sum15.Rds")
saveRDS(presvals_sum16, "../Data_Processed/prelim_models/CSTAT/pvals_sum16.Rds")
saveRDS(presvals_sum17, "../Data_Processed/prelim_models/CSTAT/pvals_sum17.Rds")
saveRDS(presvals_sum18, "../Data_Processed/prelim_models/CSTAT/pvals_sum18.Rds")
saveRDS(presvals_sum19, "../Data_Processed/prelim_models/CSTAT/pvals_sum19.Rds")

saveRDS(presvals_fall15, "../Data_Processed/prelim_models/CSTAT/pvals_fall15.Rds")
saveRDS(presvals_fall16, "../Data_Processed/prelim_models/CSTAT/pvals_fall16.Rds")
saveRDS(presvals_fall17, "../Data_Processed/prelim_models/CSTAT/pvals_fall17.Rds")
saveRDS(presvals_fall18, "../Data_Processed/prelim_models/CSTAT/pvals_fall18.Rds")
saveRDS(presvals_fall19, "../Data_Processed/prelim_models/CSTAT/pvals_fall19.Rds")

# predictors
saveRDS(pred_sum15, "../Data_Processed/prelim_models/CSTAT/pred_sum15.Rds")
saveRDS(pred_sum16, "../Data_Processed/prelim_models/CSTAT/pred_sum16.Rds")
saveRDS(pred_sum17, "../Data_Processed/prelim_models/CSTAT/pred_sum17.Rds")
saveRDS(pred_sum18, "../Data_Processed/prelim_models/CSTAT/pred_sum18.Rds")
saveRDS(pred_sum19, "../Data_Processed/prelim_models/CSTAT/pred_sum19.Rds")

saveRDS(pred_fall15, "../Data_Processed/prelim_models/CSTAT/pred_fall15.Rds")
saveRDS(pred_fall16, "../Data_Processed/prelim_models/CSTAT/pred_fall16.Rds")
saveRDS(pred_fall17, "../Data_Processed/prelim_models/CSTAT/pred_fall17.Rds")
saveRDS(pred_fall18, "../Data_Processed/prelim_models/CSTAT/pred_fall18.Rds")
saveRDS(pred_fall19, "../Data_Processed/prelim_models/CSTAT/pred_fall19.Rds")



# Graveyard ---------------------------------------------------------------

# # Create vector of unique years in occurrence dataframe
# unique_years <- unique(all_walrus$year)
# 
# # Create a for loop to subset the all_walrus df per each unique_years
# for (year in unique_years) {
#   # select all rows where the "year" column matches the current value of the
#   # "year" variable in the loop
#   walrus <- all_walrus[all_walrus$year == year, ]
#   # assign the resulting subset to a new df labeled with the name and year
#   assign(paste0("walrus", year), walrus)
# }

# Brute force method: create new summer and fall variables for each year to check
# how many occurrences fall within each season
# walrus2015_fall <- walrus2015[walrus2015$season == "fall", ]
# walrus2015_sum <- walrus2015[walrus2015$season == "summer", ]
# walrus2016_fall <- walrus2016[walrus2016$season == "fall", ]
# walrus2016_sum <- walrus2016[walrus2016$season == "summer", ]
# walrus2017_fall <- walrus2017[walrus2017$season == "fall", ]
# walrus2017_sum <- walrus2017[walrus2017$season == "summer", ]
# walrus2018_fall <- walrus2018[walrus2018$season == "fall", ]
# walrus2018_sum <- walrus2018[walrus2018$season == "summer", ]
# walrus2019_fall <- walrus2019[walrus2019$season == "fall", ]
# walrus2019_sum <- walrus2019[walrus2019$season == "summer", ]
# Some of these seasons don't contain any occurrence data. So occurrences will be
# aggregated on a yearly basis and habitat suitability will be assessed by year.
# However, the environmental data still needs to be split by season to assign the
# correct seasonal RasterStack to the season that the walrus occurrence happened.

