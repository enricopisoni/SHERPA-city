# --------------------------------------------------------------
# function creating a default extent around a city centre point
# --------------------------------------------------------------

library(rgdal)
source("long2UTM.R")

create.default.extent <- function(city.coords) {
  # extent width and height in meters (move to a config file?)
  extent.width.m <- 20000
  extent.height.m <- 20000
  # city.coords is a 1x2 matrix with longitude and latitude of the city centre
  centre.point <- SpatialPoints(city.coords, 
                                proj4string = CRS("+init=epsg:4326"))
  # conversion to UTM
  utm.string <- long2UTM(city.coords[1])
  centre.point.utm <- spTransform(centre.point, CRS(utm.string))
  # calculate SW and NE corners
  SW.coords <- matrix(c(centre.point.utm@coords[1,1] - extent.width.m/2,
                        centre.point.utm@coords[1,2] - extent.height.m/2),
                      ncol = 2)
  NE.coords <- matrix(c(centre.point.utm@coords[1,1] + extent.width.m/2,
                        centre.point.utm@coords[1,2] + extent.height.m/2),
                      ncol = 2)
  # convert both corner points back to WGS84
  corners.utm <- SpatialPoints(rbind(SW.coords, NE.coords), 
                               proj4string = CRS(utm.string))
  corners.wgs84 <- spTransform(corners.utm, CRS("+init=epsg:4326"))
  # output
  return(extent(corners.wgs84))
}


# for testing
if (1==0) {
  lon <- 9.1875
  lat <- 45.46875
  city.coords<-matrix(c(lon, lat), ncol = 2)
  create.default.extent(city.coords)
  long2UTM(8.23)
}
