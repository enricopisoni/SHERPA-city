# ------------------------------------#
# Adding a zone column to the network #
# ------------------------------------#

# input: 1) shape file with the network in UTM cropped to the domain
#        2) shape file with the zones as polygons, any projection is fine
#           two fields are compulsory: 'id' and 'zone_name'
# output: network shape file with a column 'zoneID'

library(rgdal)
library(rgeos)
library(raster)

add.zones <- function(city.utm.sldf, zones.shp, cityname) {
  # add a column with the zone name
  city.utm.sldf@data$zone_name <- NA
  
  # change the column names of the zones.shp, see later intersect
  zone.names <- as.vector(zones.shp@data$name)
  names(zones.shp@data) <- paste0(names(zones.shp@data), "_666")
  
  # get the projection string of the netowrk
  network.proj4string <- proj4string(city.utm.sldf)
  
  # convert the zones shape file in the same projection
  zones.shp.utm <- spTransform(zones.shp, network.proj4string)
  
  # make a polygon of the network extent
  extent.polygon <- as(extent(city.utm.sldf), 'SpatialPolygons')
  proj4string(extent.polygon) <- network.proj4string
  
  # determine the complementary zone polygon as the difference 
  # of the extent and all the zone polygons
  complementary.zone <- gDifference(extent.polygon, zones.shp.utm)
  
  # initialize the zoned network shape file as the network inside the
  # complementary zone
  zoned.network.shp <- intersect(city.utm.sldf, complementary.zone) # SpatialLinesDataFrame
  zoned.network.shp@data$zone_name <- "Complement"
  
  # plot(zoned.network.shp)
  color.list <- c("grey", "green", "orange", "blue")
  lgd.txt <- "Complement"
  i.col <- 1
  jpeg(file.path(city.output.folder, paste0(cityname, "_zones.jpg")), width = 2*480, height = 2*480)
  plot(zoned.network.shp, col = color.list[i.col], asp = 1)
  # To avoid double counting of roads when polygons overlap keep track of the roads
  # that are not in a zone yet.
  # If polygons overlap the roads will be assigned to the first polygon in the list.
  # rest.netork.shp <- gDifference(city.utm.sldf, zoned.network.shp) # too slow
  # rest.netork.shp <- gDifference(city.utm.sldf, complementary.zone) # faster
  zone_name <- "rand"
  for (zone_name in zone.names) {
    # convert zone SpatialDataFrame in SpatialPolygon. This avoids trouble. When a SpatialDataFrame
    # is used the data columns are attached to the result. Double names are introduced => hassle.
    zone.polygon <- as(zones.shp.utm[zones.shp.utm@data$name == zone_name,], "SpatialPolygons")
    # interesection between the network and the zone
    zone.network.shp <- intersect(city.utm.sldf, zone.polygon)
    # add the correc zone name
    zone.network.shp@data$zone_name <- zone_name
    
    # # Intersect adds the modified column names of the zones shape file: "id_666" and "zone_name_666".
    # # These are removed
    # zone.network.shp@data = subset(zone.network.shp@data, select = -c(id_666, zone_name_666))
    
    # update the network with roads without zone
    # rest.netork.shp <- gDifference(rest.netork.shp, zones.shp.utm[zones.shp.utm@data$zone_name == zone_name,])
    # update the network of roads with an assigned zone_name
    zoned.network.shp <- bind(zoned.network.shp, zone.network.shp)
    i.col <- i.col + 1
    lgd.txt <- c(lgd.txt, toString(zone_name))
    plot(zone.network.shp, col = color.list[i.col], add=TRUE)
  }
  legend("topright",  lty = 1, legend = lgd.txt, col = color.list[1:i.col])
  dev.off()
  
  return(zoned.network.shp)
}  

  # zoned.network.shp <- 
  # 
  # roads.in.zone.2 <- network.shp(zones.shp.utm, CRS(network.proj4string))
  # 
  # 
  # plot(roads.in.zone.2)
  # 
  # roads.in.zone.3 <- intersect(network.shp,
  #                              SpatialPolygons(zones.shp.utm@polygons, proj4string = CRS(proj4string(network.shp))))
  # 
  # extent.polygon <- as(extent(network.shp), 'SpatialPolygons')
  # proj4string(extent.polygon) <- CRS(proj4string(network.shp))
  # 
  # complementary.zone <- gDifference(extent.polygon,
  #                                   SpatialPolygons(zones.shp.utm@polygons, proj4string = CRS(proj4string(network.shp))))
  # plot(complementary.zone, col='blue')
  # 
  # roads.in.complementary.zone <- intersect(network.shp, complementary.zone)
  # plot(roads.in.complementary.zone)
  # head(roads.in.complementary.zone@data)
  
  


# for testing

# if (1 == 1) {
# 
#   # clean up
#   # rm(list = ls())
# 
#   wd <- "D:/SHERPAcity/NO2_atlas/run20181023/"
#   setwd(wd)
# 
#   # the network shape file
#   cityname <- "Bruxelles"
#   dsn.network <- paste0(wd, cityname)
#   layer.network <- paste0("traffic_roadlinks_", cityname)
#   network.shp <- readOGR(dsn = dsn.network, layer = layer.network)
# 
#   # the polygons shape file
#   dsn.zones <- paste0(wd, cityname, "/zones_", cityname)
#   layer.zones <- paste0("zones_", cityname)
#   zones.shp <- readOGR(dsn = dsn.zones, layer = layer.zones)
# 
#   # call the zones function
#   zone.network.shp <- add.zones(network.shp, zones.shp, cityname)
# 
# }
