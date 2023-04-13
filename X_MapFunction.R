
################################################################################
# TITLE: Marine mammal occurrence data mapping function
# PURPOSE: The purpose of this script is to provide a flexible function that can
# be used to create pretty maps of marine mammal occurrence data. 
# AUTHOR: Kelly Kapsar
# CREATED: 2023-03-06
# LAST UPDATED ON: 2023-04-10
################################################################################

# Function makeMap
# A function to map points within a particular study area in the North Pacific 
# Inputs: 
#### basemap: a shapefile containing the outlines of landmasses within the study area
#### studyarea: a shapefile containing a single polygon that defines the boundaries of the study area
#### pts: a shapefile containing the locations of observed marine mammals 
#### maptitle: a text string with the desired title for each map (No title if not specified)
#### colorcol: a text string specifying the column name used to color the points 

makeSeasonMap <- function(basemap, rangepoly, pts, maptitle=NA, colorcol="groupSz", colorname=NA){

    # Crop basemap and locations to extent of study area + 100km buffer 
    smallbbox <- st_bbox(pts)
    smallbboxsf <- bbox2sf(bbox=smallbbox, crs = st_crs(pts))
    smallbboxbuff <- st_buffer(smallbboxsf, 100000)
    basemapsmall <- basemap %>% st_crop(smallbboxbuff)
    
    p1 <- ggplot() +
      geom_sf(data=smallbboxbuff, fill=NA, color=NA, lwd=0) +
      geom_sf(data=basemapsmall, fill="#878787", lwd=0) + # Add basemap and color land green
      # coord_sf(xlim = st_coordinates(smallbboxbuff)[c(1,2),1], # min & max of x values
      #          ylim = st_coordinates(smallbboxbuff)[c(2,3),2], expand = T) + # min & max of y values
      geom_sf(data=pts, aes_string(color = {{colorcol}}), size=4) + # Add points and color by the specified column
      # scale_color_distiller(palette = "Spectral") +
      # scale_color_binned(breaks = c(2010, 2013, 2016, 2020), type = "viridis", name=colorname) +
      scale_colour_fermenter(type = "seq", 
                             palette = "Spectral",
                             breaks =  c(2010, 2012, 2014, 2016, 2018, 2020),
                             limits = c(2010,2020),
                             guide = guide_coloursteps(even.steps = TRUE,
                                                       show.limits = TRUE), 
                             name=colorname) +
      scale_x_continuous(expand = c(0, 0)) + # Remove extra white space on x axis
      scale_y_continuous(expand = c(0, 0)) + # Remove extra white space on y axis
      theme_bw() + # Set a basic them 
      ggsn::scalebar(pts, dist = 100, dist_unit = "km",
               transform = FALSE, location = "bottomright") + 
      theme(text = element_text(size = 20), # Text size = 20 for all text
            legend.position = "right",
            plot.title = element_text(hjust = 0.5), # Plot title is centered in the middle
            axis.text=element_blank(), # No axis lables 
            axis.title = element_blank(), 
            axis.ticks = element_blank(),
            panel.background = element_rect(fill = "#d6e4ee"), # Background (aka water) is blue
            panel.border =  element_rect(colour = "#d6e4ee"), # Black border around plot
            panel.grid.major = element_line(colour = "transparent")) # Remove grid lines in background 

    return(p1) # Return = output from function  
}

makeRangeMap <- function(basemap, rangepoly, pts, maptitle=NA, colorcol="groupSz", colorname=NA){
  # Crop basemap and locations to extent of study area + 100km buffer 
  rangepoly <- st_transform(rangepoly, st_crs(basemap))
  
  polybbox <- st_bbox(rangepoly)
  polybboxsf <- bbox2sf(bbox=polybbox, crs = st_crs(rangepoly))
  
  polybboxbuff <- st_buffer(polybboxsf, 100000)
  
  basemapnew <- basemap %>% st_crop(polybboxbuff)
  
  pts <- pts[!is.na(pts$year),]
  
  p0 <- ggplot() +
    geom_sf(data=rangepoly, fill=NA, color="#e31a1c", lwd=1.5) + # Add basemap and color land green
    geom_sf(data=basemapnew, fill="#878787", lwd=0) + # Add basemap and color land green
    geom_sf(data=pts, aes_string(color = {{colorcol}}), size=3) + # Add points and color by the specified column
    # scale_color_distiller(palette = "Spectral") +
    # scale_color_binned(breaks = c(2010, 2013, 2016, 2020), type = "viridis", name=colorname) +
    scale_colour_fermenter(type = "seq", 
                           palette = "Spectral",
                           breaks =  c(2010, 2012, 2014, 2016, 2018, 2020),
                           limits = c(2010,2020),
                           guide = guide_coloursteps(even.steps = TRUE,
                                                     show.limits = TRUE), 
                           name=colorname) +
    scale_x_continuous(expand = c(0, 0)) + # Remove extra white space on x axis
    scale_y_continuous(expand = c(0, 0)) + # Remove extra white space on y axis
    labs(fill=colorname) +
    ggsn::scalebar(rangepoly, dist = 500, dist_unit = "km",
                   transform = FALSE, location = "bottomright") + 
    theme_bw() + # Set a basic them 
    theme(text = element_text(size = 20), # Text size = 20 for all text
          plot.title = element_text(hjust = 0.5), # Plot title is centered in the middle
          axis.text=element_blank(), # No axis lables 
          axis.ticks=element_blank(),
          legend.position="right",
          panel.background = element_rect(fill = "#d6e4ee"), # Background (aka water) is blue
          panel.border =  element_rect(colour = "black"), # Black border around plot 
          panel.grid.major = element_line(colour = "transparent")) # Remove grid lines in background 
  
  # Add title if specified 
  if(!is.na(maptitle)){
    p0 <- p0 + ggtitle(label = maptitle) 
  }
  return(p0)
}



