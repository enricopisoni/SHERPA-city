# --------------------
# check zone overlap
# --------------------

library(rgdal)
library(rgeos)

check.zones.overlap <- function(zones.shp) {
  # initialize
  overlap <- FALSE
  if (!("name" %in% names(zones.shp@data))) {
    print("WARNING: no field name in the zones shape file.")
  }
  zone.names <- as.vector(zones.shp@data$name)
  n.zones <- length(zone.names)
  if (n.zones == 1) {
    overlap <- FALSE
  } else {
    # i.zone <- 1   # for debugging
    # j.zone <- 2   # for debugging
    # loop over zones from first to the forlast
    for (i.zone in 1:(n.zones-1)) {
      zone.name.i <- zone.names[i.zone]
      zone.i.polygon <- as(zones.shp[zones.shp@data$name == zone.name.i,], "SpatialPolygons")
      # loop from the next zone to the last one
      for (j.zone in (i.zone+1):n.zones) {
        zone.name.j <- zone.names[j.zone]
        zone.j.polygon <- as(zones.shp[zones.shp@data$name == zone.name.j,], "SpatialPolygons")
        # plots for debugging
        # plot(extent(zones.shp))
        # plot(zone.i.polygon, add=T)
        # plot(zone.j.polygon, add=T)
        options(warn=-1) # just supress the no intersection warning
        intersect.ij <- intersect(zone.i.polygon, zone.j.polygon)
        options(warn=0)
        if (!is.null(intersect.ij)) {
          print(paste0("Zones ", zone.name.i, " and ", zone.name.j, " overlap."))
          overlap <- TRUE
        } # if there is an overlap
      } # loop from the next zone to the last one
    } # loop over all zones from first to forelast
  } # more then one zone
  
  return(overlap)
}

if (1 == 0) {
  # for testing
  wd <- "D:/SHERPAcity/NO2_atlas/run20181023/"
  setwd(wd)
  cityname <- "Bruxelles"
  
  # clearly not-overlapping polygons > FALSE
  dsn.zones.shp.file <- paste0(cityname, "/zones_", cityname) 
  layer.zones.shp.file <- paste0("zones_", cityname)
  zones.shp.file <- paste0(dsn.zones.shp.file, "/", layer.zones.shp.file, ".shp")
  zones.shp <- readOGR(dsn = dsn.zones.shp.file, layer = layer.zones.shp.file)
  check.zones.overlap(zones.shp)
  
  # overlapping polygons: overlap > TRUE
  dsn.zones.shp.file <- paste0(cityname, "/zones_", cityname, "_with_overlap") 
  layer.zones.shp.file <- paste0("zones_", cityname)
  zones.shp.file <- paste0(dsn.zones.shp.file, "/", layer.zones.shp.file, ".shp")
  zones.shp <- readOGR(dsn = dsn.zones.shp.file, layer = layer.zones.shp.file)
  check.zones.overlap(zones.shp)
  
  # touching polygons: overlap > FALSE
  dsn.zones.shp.file <- paste0(cityname, "/zones_", cityname, "_touching") 
  layer.zones.shp.file <- paste0("zones_", cityname)
  zones.shp.file <- paste0(dsn.zones.shp.file, "/", layer.zones.shp.file, ".shp")
  zones.shp <- readOGR(dsn = dsn.zones.shp.file, layer = layer.zones.shp.file)
  check.zones.overlap(zones.shp)
}

  