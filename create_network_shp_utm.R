# --------------------------------------------
# create network shape file of a domain in UTM
# --------------------------------------------

# This script checks which boundary boxes of OTM files overlap with the domain.
# The overlapping OTM shape files are opened, cropped to the domain and joined together.
# The result is a shape file of the road network in the domain.

library(rgdal)
library(raster)
library(rgeos)
source("long2UTM.R")

create.network.shp.utm <- function(city.extent, cityname, nuts3.bbox.df, otm.path) {
  # list with boundary boxes of nuts3 areas
  # loop over the boundary box file and check which boundary boxes overlap with the domain
  nuts3.in.city.domain <- c()
  for (i in 1:NROW(nuts3.bbox.df)) {
    extent.nuts3 <- extent(c(nuts3.bbox.df$lon_min[i], nuts3.bbox.df$lon_max[i],
                             nuts3.bbox.df$lat_min[i], nuts3.bbox.df$lat_max[i]))
    if (!is.null(intersect(extent.nuts3, city.extent))) {
      nuts3 <- toString(nuts3.bbox.df$nuts3[i])
      nuts3.in.city.domain <- c(nuts3.in.city.domain, nuts3)
    }
  }
  # initialize shape file with the union of all OTM files inside the domain
  shp.union <- NULL
  for (nuts3 in nuts3.in.city.domain) {
    nuts3.shp.folder <- file.path(otm.path, nuts3)
    nuts3.shp.name <- paste0("traffic_roadlinks_", nuts3)
    shp.orig <- readOGR(dsn = nuts3.shp.folder, layer = nuts3.shp.name)
    shp.cropped <- crop(shp.orig, city.extent)
    if (is.null(shp.union)) {
      shp.union <- shp.cropped
    } else if (!is.null(shp.cropped)) {
      shp.union <- bind(shp.union, shp.cropped)
    }
  }
  # convert to UTM
  utm.string <- long2UTM(mean(city.extent@xmin, city.extent@xmax))
  shp.utm <- spTransform(shp.union, CRS(utm.string))
  
  # plot the domain
  png(file.path(city.output.folder, paste0(cityname, "_utm.png")), width = 4*480, height = 4*480,
      pointsize = 4*12)
  plot(extent(shp.utm), col="black", xlab = "x (m)", ylab = "y (m)", main = cityname, asp = 1)
  functional.list <- c('mainRoad', 'firstClass', 'secondClass', 'thirdClass', 'fourthClass')
  functional.color <- c("red", "blue", "light blue", "green", "light green")
  functional.lwd <- c(5, 4, 3, 2, 1)
  for (i.f in 1:5) {
    functional <- functional.list[i.f]
    plot(shp.utm[shp.utm@data$functional == functional,], col = functional.color[i.f], 
         lwd = functional.lwd[i.f], add = TRUE)
  }
  dev.off()
  
  # return the result  
  return(shp.utm)
  
}

