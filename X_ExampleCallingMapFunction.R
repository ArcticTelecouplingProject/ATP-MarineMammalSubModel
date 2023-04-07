
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

# Load in makeMap function from MapFunction.R script
source("./X_MapFunction.R")

# Load in data to test function 
bmap <- st_read("../ATP-MarineMammal-DataRaw/base_map/AK_CAN_RUS.shp") %>% st_transform(3338)
locs <- st_read("../Data_Processed/1-1_ARCSS_GBIF_walrus/1-1_walrus_gbif_sp.shp") %>% st_transform(3338)
rangepolys <- st_read("../OdobenusRosmarus_ExtentOfRange.shp") 
rangepoly <- rangepolys %>% dplyr::filter(study_desc == "Summer and fall extent of range")

# Run function with inputs described in "MapFunction.R" script. 
testmap <- makeMap(basemap=bmap, 
                   rangepoly = rangepoly,
                   pts=locs, 
                   maptitle = "Walrus locations", 
                   colorcol = "year", 
                   colorname = "Year")

# View output
testmap

# Save output 
ggsave(filename = "../Figures/WalrusLocations", plot = testmap, width=10, height=8, units="in")

