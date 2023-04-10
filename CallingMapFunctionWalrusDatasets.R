################################################################################
# TITLE: Mapping marine mammal occurrence 
# PURPOSE: The purpose of this script is to show proof of concept for the mapping
# function developed in the MapFunction.R script. 
# AUTHOR: Kelly Kapsar
# CREATED: 2023-03-07
# LAST UPDATED ON: 2023-03-07
################################################################################

##### Preliminaries #####

# Load libraries
library(sf) # For vector data (points, lines, polygons) 
library(tidyverse) # For data manipulation 
library(pgirmess) # for bounding box 
library(RColorBrewer)

# Load in makeMap function from MapFunction.R script
source("./X_MapFunction.R")

# Set up data directory
datadir <- "C:/Users/Natalia Portales/Michigan State University/Waloven, Sydney - ATP-MarineMammal-DataRaw/"
datadir <- "../../ATP-MarineMammal-DataRaw/"

# Load in data to test function 
bmap <- st_read(paste0(datadir, "base_map/AK_CAN_RUS.shp"))
bmap <- bmap %>% st_transform(3338)

rangepolys <- st_read(paste0(datadir, "OdobenusRosmarus_ExtentOfRange/OdobenusRosmarus_ExtentOfRange.shp"))
rangepoly <- rangepolys %>% dplyr::filter(study_desc == "Summer and fall extent of range")

# Run function for summer locations 
locs_summer <- st_read("../Data_Processed/Odobenus_DataIntegration_20230116/Summer/june_mapping_odobenus_integrated20230210.shp")
testseason <- makeSeasonMap(basemap=bmap, 
                   rangepoly = rangepoly,
                   pts=locs_summer, 
                   maptitle = NA, 
                   colorcol = "year", 
                   colorname = "Year")
testseason
ggsave(testseason, filename = "../Figures/WalrusLocations_Summer", plot = testmap, width=10, height=8, units="in")

# Run function for fall locations
locs_fall <- st_read("../Data_Processed/Odobenus_DataIntegration_20230116/Fall/sep_mapping_odobenus_integrated20230210.shp")
testseason <- makeSeasonMap(basemap=bmap, 
                            rangepoly = rangepoly,
                            pts=locs_fall, 
                            maptitle = NA, 
                            colorcol = "year", 
                            colorname = "Year")
testseason
ggsave(testseason, filename = "../Figures/WalrusLocations_Fall", plot = testmap, width=10, height=8, units="in")

# Run function with inputs described in "MapFunction.R" script. 
alllocs <- rbind(locs_fall, locs_summer)
testrange <- makeRangeMap(basemap=bmap, 
                   rangepoly = rangepoly,
                   pts=alllocs, 
                   maptitle = NA, 
                   colorcol = "year", 
                   colorname = "Year")
testrange

# Save output 
ggsave(testrange, filename = "../Figures/WalrusRange", plot = testmap, width=10, height=8, units="in")
