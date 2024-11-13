# Title: Marine Mammal Data Paper Tidying 
# Author: Sydney Waloven
# Date: 2024-11-11
# Description: BLM REA SNK 2010 TS Mammals, ecoregion SNK
# Bureau of Land Management https://www.sciencebase.gov/catalog/item/5d81070ce4b0c4f70d057fa0

# This script follows a standardized method for data tidying discussed in our data paper.

# setup -------------------------------------------------------------------

# Load libraries
library(sf)
library(tidyverse)
library(tidyjson)

# Clear environment history 
rm(list=ls())

# Import data
# data_set <- read_csv("pathway/to/data/set.csv")

# This example will use the datadir method that we used as a workaround for the file path issue of the raw data.
datadir <- "C:/Users/Natalia Portales/Michigan State University/Waloven, Sydney - ATP-MarineMammal-DataRaw/"
datadir <- "../../ATP-MarineMammal-DataRaw/"

# This script will tidy marine mammal data from the BLM REA SNK 2010 TS Mammals.

# Occurrence data
pinniped_occ <- jsonlite::fromJSON(paste0(datadir, "23_blm_rea_snk_2010/blm_rea_snk_2010.json"))
json <- jsonlite::fromJSON(paste0(datadir, "23_blm_rea_snk_2010/blm_rea_snk_2010.json"))
## Use tidyjson?

# # To convert to a CSV and tidy:
# # Initialize output data frame
# df <- data.frame()
# 
# new_row <- data.frame(
#   link_rel = json[["link"]][["rel"]], # nested list
#   link_url = json[["link"]][["url"]], # nested list
#   relatedItems_url = json[["relatedItems"]][["link"]][["url"]], # nested list
#   relatedItems_rel = json[["relatedItems"]][["link"]][["rel"]], # nested list
#   id = json[["id"]],
#   identifiers_type = json[["identifiers"]][["type"]], # nested list
#   identifiers_scheme = json[["identifiers"]][["scheme"]], # nested list
#   identifiers_key = json[["identifiers"]][["key"]], # nested list
#   title = json[["title"]],
#   summary = json[["summary"]],
#   body = json[["body"]],
#   provenance_dateCreated = json[["provenance"]][["dateCreated"]], # nested list
#   provenance_lastUpdated = json[["provenance"]][["lastUpdated"]], # nested list
#   hasChildren = json[["hasChildren"]],
#   parentId = json[["parentId"]],
#   browseTypes = json[["browseTypes"]],
#   spatial = json[["spatial"]][["boundingBox"]], # nested list
#   distributionLinks = json[["distributionLinks"]],
#   previewImage = json[["previewImage"]]
# )
#   
# # Handle nested data frames in 'webLinks' and 'tags'
# new_row$webLinks <- paste(json[["webLinks"]][["uri"]], collapse = "; ")
# new_row$tags <- paste(json[["tags"]][["name"]], collapse = "; ")
# 
# # Add the new row to the main data frame
# df <- rbind(df, new_row)
# 
# # View structure
# str(pinniped_occ)

