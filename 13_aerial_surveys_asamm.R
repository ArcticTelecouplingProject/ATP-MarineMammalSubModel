# 31May2023
# Author: Sydney Waloven

# Source: NOAA
# Aerial Surveys of Arctic Marine Mammals (ASAMM) collected by Marine Mammal Laboratory,
# Bureau of Ocean Energy Management, and other agencies in the Bering, Chukchi,
# Beaufort Seas, and Amundsen Gulf from 1979 to 2019 and North Slope Borough, Alaska
# from 2020 to 2021
# https://www.ncei.noaa.gov/archive/archive-management-system/OAS/bin/prd/jquery/accession/download/39614

# Species isolated in this dataset: 

# This dataset was tidied and ready for data integration on 
# Last edited: 


# Libraries and Packages --------------------------------------------------

library(terra)
library(sf)
library(tidyverse)
library(readr)

# Clear history 
rm(list=ls())


# ASAMM Surveys -----------------------------------------------------------

# Import raw walrus occurrence data
datadir <- "../../ATP-MarineMammal-DataRaw/"

# Specify the file path to the tar.gz file
file_path <- paste0(datadir, "13_aerial_surveys_asamm/0039614.11.11.tar.gz")

# Extract the contents of the tar.gz file
# extract_files <- untar(file_path)
# Moved output file "0039614" into Data_Raw folder

comida <- st_read(paste0(datadir, "13_aerial_surveys_asamm/0039614/11.11/data/0-data/ASAMM_Blocks/COMIDA23.shp"))

plot(comida)

