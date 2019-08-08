# ---------------------------------------------
# Extract SHERPAcity results in format for maps
# ---------------------------------------------

# Clean up
rm(list=ls())
# set the directory of this script as working directory
wd <- dirname(sys.frame(1)$ofile)
# wd<-"D:/SHERPAcity/NO2_atlas/run20190731_SNAP7zero"
setwd(wd)

library(raster)
library(rgdal)
library(rgeos)
library(plyr)
library(ggplot2)
library(parallel)
source("long2UTM.R")
source("NO2_atlas_config.R")

# for plots
ircel.colors <- c("#0000FF", "#007EFD", "#00C000", "#00FF00", "#CCFF33", 
                  "#F8E748", "#FF8000", "#FF0000", "#C00000", "#800000", "#660099")
my.levels <- c(0, 10, 15, 20, 25, 30, 35, 40, 50, 60, 70, 80)
n.levels <- length(my.levels)
my.labels <- sapply(my.levels, toString)
my.labels[n.levels] <- paste0(">", my.labels[n.levels-1])

getColor <- function(x, the.levels, the.colors) {
  if (is.na(x)) {
    x.color <- "#FFFFFF"
  } else {
    n.lev <- length(the.levels)
    i <- 1
    while ((x >= the.levels[i]) & (i < n.lev)) {
      x.color <- the.colors[i]
      i <- i+1
    }
  }
  return(x.color)
}

# list of cites
cities.list <- as.vector(city.df$cityname)

# read file with monitoring stations positions
stations.file <- "../_measurements/Airbase_NO2_NO_2015.csv"
stations.df <- read.table(stations.file, sep = ",", header = TRUE, quote = "")

# the file contains NO2 and NO measurements. Add NOx and remove NO
NO2.stations.df <- stations.df[stations.df$Pollutant == "Nitrogen dioxide (air)",]
NROW(NO2.stations.df) - length(unique(NO2.stations.df$SamplingPointLocalId))

NO.stations.df <- stations.df[stations.df$Pollutant == "Nitrogen monoxide (air)",]
NROW(NO.stations.df) - length(unique(NO.stations.df$SamplingPointLocalId))
NOx.stations.df <- merge(NO2.stations.df, NO.stations.df, 
                         by = c("Country", "ReportingYear", "StationLocalId", 
                                "SamplingPoint_Latitude", "SamplingPoint_Longitude",
                                "AggregationType", "StationType"))
NOx.stations.df$AQValue <- NOx.stations.df$AQValue.x + NOx.stations.df$AQValue.y
NOx.stations.df$Pollutant <- "Nitrogen oxides (air)"
NOx.stations.df$SamplingPointLocalId <- NA
NOx.stations.df <- NOx.stations.df[, names(stations.df)]
stations.df <- rbind(stations.df, NOx.stations.df)

# make a spatial points data.frame
stations.spdf <- SpatialPointsDataFrame(coords = stations.df[, c("SamplingPoint_Longitude", "SamplingPoint_Latitude")],
                                        data = stations.df,
                                        proj4string = CRS("+proj=longlat +ellps=WGS84"))

# compare measurements and modelled concentrations
for (cityname in cities.list) {
# city.validation <- function(cityname) {
  print(cityname)
  # city coordinates and CRS
  city.coords <- matrix(c(city.df$lon[city.df$cityname == cityname],
                          city.df$lat[city.df$cityname == cityname]), ncol = 2,
                        dimnames = list(c(cityname), c("lon", "lat")))
  city.epsg <- CRS(long2UTM(city.coords[1,"lon"]))
  
  # all stations in UTM of the current city
  stations.utm <- spTransform(stations.spdf, city.epsg)
  
  # check if there are results, make a list of the scenarios and local_NOx_conc files
  city.output.folder <- sub("^/", "", file.path(cities.output.folder, cityname))
  city.results.folder <- file.path(city.output.folder, "results")
  basecase.folder <- file.path(city.results.folder, "basecase")
  NO2.basecase.asc <- file.path(basecase.folder, "NO2_total_conc.asc")
  NOx.basecase.asc <- file.path(basecase.folder, "NOx_total_conc.asc")
  
  # it the basecase NO2 exists
  if (file.exists(NO2.basecase.asc)) {
    # read NO2 and NOx raster file
    NO2.basecase.r <- raster(NO2.basecase.asc)
    NOx.basecase.r <- raster(NOx.basecase.asc)
    
    # get the extent and convert to WGS 84
    extent.polygon.utm <- as(extent(NO2.basecase.r), 'SpatialPolygons')
    proj4string(extent.polygon.utm) <- city.epsg #city.projection
    extent.polygon.wgs84 <- spTransform(extent.polygon.utm, CRS("+proj=longlat +ellps=WGS84"))
    
    # get stations inside the domain
    station.in.domain <- gIntersects(extent.polygon.wgs84, stations.spdf, byid = TRUE)
    # if (sum(station.in.domain)>0) {}
    city.stations.wgs84.spdf <- SpatialPointsDataFrame(coords = matrix(stations.spdf@coords[station.in.domain,], 
                                                                       ncol=2),
                                                       data = stations.spdf@data[station.in.domain,],
                                                       proj4string = CRS("+proj=longlat +ellps=WGS84"))
    
    city.stations.utm.spdf <- spTransform(city.stations.wgs84.spdf, city.epsg) # city.projection
    # add a letter code for each station
    city.station.vec <- as.vector(unique(city.stations.utm.spdf@data$StationLocalId))
    station2letter.df <- data.frame(StationLocalId = city.station.vec,
                                    letter.code = LETTERS[1:length(city.station.vec)])
    city.stations.utm.spdf@data <- merge(city.stations.utm.spdf@data, station2letter.df)
    
    # extract NO, NO2 and NOx concentrations for stations
    n.measurements <- NROW(city.stations.utm.spdf@data)
    city.stations.utm.spdf@data$AQValueModel <- NA
    
    # extract model concentrations at measurement locations
    for (i.meas in 1:n.measurements) {
      Pollutant <- toString(city.stations.utm.spdf@data$Pollutant[i.meas])
      station.utm.coord <- matrix(city.stations.utm.spdf@coords[i.meas,], ncol = 2)
      if (Pollutant == "Nitrogen dioxide (air)") {
        city.stations.utm.spdf@data$AQValueModel[i.meas] <- extract(NO2.basecase.r, station.utm.coord)
      } else if (Pollutant == "Nitrogen monoxide (air)") {
        NO.model <- extract(NOx.basecase.r, station.utm.coord) - extract(NO2.basecase.r, station.utm.coord)
        city.stations.utm.spdf@data$AQValueModel[i.meas] <- NO.model
      } else if (Pollutant == "Nitrogen oxides (air)") {
        city.stations.utm.spdf@data$AQValueModel[i.meas] <- extract(NOx.basecase.r, station.utm.coord)
      }
    } # loop over stations
    
    # validation plot: measured versus modelled NOx, NO and NO2
    p <- ggplot(data = city.stations.utm.spdf@data, 
                aes(x=AQValue, y=AQValueModel, col = StationType, label = letter.code))
    p <- p + geom_point() + facet_grid(. ~ Pollutant) + geom_abline(intercept = 0, slope = 1)
    p <- p + theme(text = element_text(size=20)) + geom_text(hjust = 0, nudge_x = 1)
    n.pollutant <- length(unique(city.stations.utm.spdf@data$Pollutant))
    png(file.path("_validation", paste0(cityname, "_validation.png")), 
        height = 480, width = n.pollutant*480)
    print(p)
    dev.off()
    
    # write the validation data to a table
    write.table(city.stations.utm.spdf@data,
                file = file.path("_validation", paste0(cityname, "_validation.csv")), 
                sep = ",", row.names = F)
    
    # plot of NO2 with stations
    # -------------------------
    median.no2 <- round(median(values(NO2.basecase.r), na.rm = T))
    # breaks and labels
    no2.levels <- round(my.levels / min(10, (35/median.no2)), 1)
    top.level <- no2.levels[n.levels-1]
    no2.labels <- sapply(no2.levels, toString)
    no2.labels[n.levels] <- paste0(">", no2.labels[n.levels-1])
    # put all values above the top level to top.level+1 (to have a nicer scale)
    NO2.basecase.r[NO2.basecase.r>top.level] <- top.level + 0.001
    
    # open the road network of the city
    # folder and filename of the UTM shape file with the network and zones
    dsn.zoned.city.utm.shp <- file.path(cities.network.folder, cityname)
    layer.city.zoned.utm.shp <- paste0("traffic_roadlinks_zones_", cityname)
    zoned.city.utm.shp <- paste0(dsn.zoned.city.utm.shp, "/", layer.city.zoned.utm.shp, ".shp")
    # read the network as a SpatialLinesDataFrame
    city.utm.sldf <- readOGR(dsn = dsn.zoned.city.utm.shp, layer = layer.city.zoned.utm.shp)
    
    # open the zones file
    dsn.zones.shp <- file.path(cities.zones.folder, cityname, paste0("/zones_", cityname)) 
    layer.zones.shp <- paste0("zones_", cityname)
    zones.shp <- file.path(dsn.zones.shp, paste0(layer.zones.shp, ".shp"))
    zones.spdf <- readOGR(dsn = dsn.zones.shp, layer = layer.zones.shp)
    zones.utm.spdf <- spTransform(zones.spdf, city.epsg) # city.projection  
    
    no2.stations <- city.stations.utm.spdf@data$Pollutant == "Nitrogen dioxide (air)"
    station.colors <- sapply(city.stations.utm.spdf@data$AQValue[no2.stations], 
                             getColor, no2.levels, ircel.colors)
    
    # make a map with the local NO2, zones and stations
    tiff(paste0("_validation/", cityname, "_NO2_stations.tiff"), 
         width = 2*480, height = 2*480, res = 144)
    plot(NO2.basecase.r, 
         breaks = no2.levels,
         col = ircel.colors,
         lab.breaks = no2.labels,
         legend.args=list(text=expression('NO'[2]*' ('*mu*'g/m'^3*')')), # , side=4, font=2, line=2.5, cex=0.8
         bty = 'n', xaxt = 'n', yaxt = 'n',
         main=paste0(cityname)) # alpha = 0.5, "Stations in ", 
    plot(city.utm.sldf, col="grey", add=T)
    plot(zones.utm.spdf, border = "black", add=T)
    # plot(city.stations.utm.spdf, add=T, pch=19)
    if (sum(no2.stations) > 0) { 
      text(x = city.stations.utm.spdf@coords[no2.stations,], pos = 3,
           labels = city.stations.utm.spdf@data$letter.code[no2.stations])
      # add points with the same color scale for the stations
      points(city.stations.utm.spdf@coords[no2.stations,], pch=19, col=station.colors)
      points(city.stations.utm.spdf@coords[no2.stations,]) # black circles
    }
    dev.off()

    # plot of NOx with stations
    # -------------------------
    median.nox <- round(median(values(NOx.basecase.r), na.rm = T))
    # breaks and labels
    nox.levels <- round(my.levels / min(10, (35/median.nox)), 1)
    top.level <- no2.levels[n.levels-1]
    nox.labels <- sapply(nox.levels, toString)
    nox.labels[n.levels] <- paste0(">", nox.labels[n.levels-1])
    # put all values above the top level to top.level+1 (to have a nicer scale)
    NOx.basecase.r[NOx.basecase.r>top.level] <- top.level + 0.001
    
    nox.stations <- city.stations.utm.spdf@data$Pollutant == "Nitrogen oxides (air)"
    station.colors <- sapply(city.stations.utm.spdf@data$AQValue[nox.stations], 
                             getColor, nox.levels, ircel.colors)
    
    # make a map with the local NOx, zones and stations
    tiff(paste0("_validation/", cityname, "_NOx_stations.tiff"), 
         width = 2*480, height = 2*480, res = 144)
    plot(NOx.basecase.r, 
         breaks = nox.levels,
         col = ircel.colors,
         lab.breaks = nox.labels,
         legend.args=list(text=expression('NO'[x]*' ('*mu*'g/m'^3*')')), # , side=4, font=2, line=2.5, cex=0.8
         bty = 'n', xaxt = 'n', yaxt = 'n',
         main=paste0(cityname)) # alpha = 0.5, "Stations in ", 
    plot(city.utm.sldf, col="grey", add=T)
    plot(zones.utm.spdf, border = "black", add=T)
    # plot(city.stations.utm.spdf, add=T, pch=19)
    if (sum(nox.stations) > 0) {    
      text(x = city.stations.utm.spdf@coords[nox.stations,], pos = 3,
           labels = city.stations.utm.spdf@data$letter.code[nox.stations])
      # add points with the same color scale for the stations
      points(city.stations.utm.spdf@coords[nox.stations,], pch=19, col=station.colors)
      points(city.stations.utm.spdf@coords[nox.stations,]) # black circles
    }
    dev.off()
  } # if file exists
} # loop over cities

city.validation("Antwerpen")
