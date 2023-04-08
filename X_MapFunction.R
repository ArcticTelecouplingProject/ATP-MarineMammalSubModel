
################################################################################
# TITLE: Marine mammal occurrence data mapping function
# PURPOSE: The purpose of this script is to provide a flexible function that can
# be used to create pretty maps of marine mammal occurrence data. 
# AUTHOR: Kelly Kapsar
# CREATED: 2023-03-06
# LAST UPDATED ON: 2023-03-06
################################################################################

# Function makeMap
# A function to map points within a particular study area in the North Pacific 
# Inputs: 
#### basemap: a shapefile containing the outlines of landmasses within the study area
#### studyarea: a shapefile containing a single polygon that defines the boundaries of the study area
#### pts: a shapefile containing the locations of observed marine mammals 
#### maptitle: a text string with the desired title for each map (No title if not specified)
#### colorcol: a text string specifying the column name used to color the points 

makeMap <- function(basemap, rangepoly, pts, maptitle=NA, colorcol="groupSz", colorname=NA){
  # Crop basemap and locations to extent of study area + 100km buffer 
  rangepoly <- st_transform(rangepoly, st_crs(basemap))
  
  polybbox <- st_bbox(rangepoly)
  polybboxsf <- bbox2sf(bbox=polybbox, crs = st_crs(rangepoly))
  basemapnew <- basemap %>% st_crop(polybboxsf)
  
  pts <- pts[-which(is.na(pts$year)),]

  p0 <- ggplot() +
    geom_sf(data=rangepoly, fill=NA, color="#e31a1c", lwd=0.5) + # Add basemap and color land green
    geom_sf(data=basemapnew, fill="#878787", lwd=0) + # Add basemap and color land green
    geom_sf(data=pts, aes_string(color = {{colorcol}}), size=6) + # Add points and color by the specified column
    scale_color_continuous(breaks = c(1900, 1950, 2000, 2020), type = "viridis", name=colorname) +
    scale_x_continuous(expand = c(0, 0)) + # Remove extra white space on x axis
    scale_y_continuous(expand = c(0, 0)) + # Remove extra white space on y axis
    labs(fill=colorname) +
    theme_bw() + # Set a basic them 
    theme(text = element_text(size = 20), # Text size = 20 for all text
          plot.title = element_text(hjust = 0.5), # Plot title is centered in the middle
          axis.text=element_blank(), # No axis lables 
          panel.background = element_rect(fill = "#d6e4ee"), # Background (aka water) is blue
          panel.border =  element_rect(colour = "black"), # Black border around plot 
          panel.grid.major = element_line(colour = "transparent")) # Remove grid lines in background 

  # Add title if specified 
  if(!is.na(maptitle)){
    p0 <- p0 + ggtitle(label = maptitle) 
  }
  
  return(p0) # Return = output from function
}
