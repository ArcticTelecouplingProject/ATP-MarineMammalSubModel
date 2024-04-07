# Title: NOAA Spotted Seal Status Review
# Author: Sydney Waloven
# Date: 2024-04-06
# Description: Data obtained from: https://ak.audubon.axds.co/#module-metadata/8ddeaf2e-5d81-11e9-8198-0023aeec7b98

# This dataset contains information on relative abundance and seasonal distribution of spotted seals within the project area. Spotted seal general distribution data were provided in the most recent NOAA status review (Boveng et al. 2009). The year-round data were from National Oceanic and Atmospheric Administration (1988). Winter/spring and summer/fal relative abundance is derived from National Oceanic and Atmospheric Administration (1988).Spotted seal haulouts were compiled from Lowry et al. (1998); National Oceanic and Atmospheric Administration (1988); and National Oceanic and Atmospheric Administration (2005). Additional information on the methodology for creating datasets in this database as well as references can be found in the Mapping Methods section of the Ecological Atlas of the Bering, Chukchi, and Beaufort Seas and at http://ak.audubon.org.

# Data number 33.
# No occurrence points. Only relative abundance polygons.


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
noaa <- read_csv(paste0(datadir, "additional_data/33_audubon_noaa_largha_haulouts/audubon_spottedseal_polys_win_spr.csv"))

noaa_sum <- read_csv(paste0(datadir, "additional_data/33_audubon_noaa_largha_haulouts/audubon_spottedseal_sum_fall.csv"))

