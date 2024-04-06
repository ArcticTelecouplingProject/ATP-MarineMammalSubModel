# Title: Observations of Marine Mammals of Beringia 2008
# Author: Sydney Waloven
# Date: 2024-04-05
# Description: Data obtained from: https://arcticdata.io/catalog/view/doi%3A10.5065%2FD6DV1GVN

# Visual line-transect surveys for cetaceans were conducted in the southeastern Bering Sea (SEBS) from 10 June to 3 July 2000, in association with a pollock stock assessment survey aboard the NOAA ship Miller Freeman.

# Data number 25. Tidy script number 18


# setup -------------------------------------------------------------------

# Load libraries
library(sf)
library(tidyverse)

# Clear environment history 
rm(list = ls())

# Set datadir
datadir <- "C:/Users/Natalia Portales/Michigan State University/Waloven, Sydney - ATP-MarineMammal-DataRaw/"
datadir <- "../../ATP-MarineMammal-DataRaw/"

# Read in csv
nsf_2008 <- read_csv(paste0(datadir, "18_nsf_mm_beringia_2008/data/HLY0801-FWS-Wal-obs-3.15-3.25.csv"))


# data tidying ------------------------------------------------------------

# Check out column names
names(nsf_2008)

walrus <- nsf_2008 %>% 
  
  # Drop NAs in coordinates
  drop_na()
