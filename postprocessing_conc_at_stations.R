# ---------------------------------------------
# Extract SHERPAcity results in format for maps
# ---------------------------------------------

# Clean up
rm(list=ls())
# set the directory of this script as working directory
wd <- dirname(sys.frame(1)$ofile)
setwd(wd)

library(raster)
library(rgdal)
library(rgeos)
library(plyr)
library(ggplot2)
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

# read file with monitoring stations positions
stations.file <- "_postprocessing/EEA_measurement_stations.txt"
stations.df <- read.table(stations.file, sep = "\t", header = TRUE, quote = "")
stations.spdf <- SpatialPointsDataFrame(coords = stations.df[, c("Longitude", "Latitude")],
                                        data = stations.df[, c("City", "AirQualityStationEoICode", "AirPollutionLevel",
                                                               "AQStationName", "Longitude", "Latitude")],
                                        proj4string = CRS("+proj=longlat +ellps=WGS84"))

# list of cites
cities.list <- as.vector(city.df$cityname)

# cityname <- "Ljubljana"
# loop over all cities
for (cityname in cities.list) {
  
  city.output.folder <- sub("^/", "", file.path(cities.output.folder, cityname))
  
  # city coordinates and CRS
  city.coords <- matrix(c(city.df$lon[city.df$cityname == cityname],
                          city.df$lat[city.df$cityname == cityname]), ncol = 2,
                        dimnames = list(c(cityname), c("lon", "lat")))
  city.epsg <- CRS(long2UTM(city.coords[1,"lon"]))
  
  # open the road network of the city
  # folder and filename of the UTM shape file with the network and zones
  dsn.zoned.city.utm.shp <- file.path(cities.network.folder, cityname)
  layer.city.zoned.utm.shp <- paste0("traffic_roadlinks_zones_", cityname)
  zoned.city.utm.shp <- paste0(dsn.zoned.city.utm.shp, "/", layer.city.zoned.utm.shp, ".shp")
  # read the network as a SpatialLinesDataFrame
  city.utm.sldf <- readOGR(dsn = dsn.zoned.city.utm.shp, layer = layer.city.zoned.utm.shp)
  
  
  # check if there are results, make a list of the scenarios and local_NOx_conc files
  city.results.folder <- file.path(city.output.folder, "results")
  if (dir.exists(city.results.folder)) {
    local.nox.files <- list.files(path = city.results.folder, recursive = T, pattern = "NOx_local_conc.asc")
    n.scens <- length(local.nox.files)
    scenario.list <- unlist(strsplit(local.nox.files, "/"))[seq(from = 1, to = 2*n.scens, by = 2)]
  } else {
    n.scens <- 0
    scenario.list <- c()
    local.nox.files <- c()
  }
  
  # read the scenario definition file
  # scenario.definition.file <- paste0(cityname, "/", cityname, "_scenario_definition.csv")
  # scenario.definition.df <- read.table(scenario.definition.file, sep = ",", header = T)
  
  # If there are results, get values at the stations
  if (length(scenario.list) > 0) {
    print(paste0("Postprocessing results of ", cityname))
    # get the UTM projection (can be done quicker with the lat lon of the city)
    # network.shp <- readOGR(dsn = city.folder,
    #                        layer = paste0("traffic_roadlinks_zones_", cityname))
    # city.projection <- proj4string(network.shp)
    
    # data frame for the result at stations
    results.at.stations.df <- data.frame()
    # i.scen <- 1 # for testing
    for (i.scen in 1:n.scens) {
      scenario.name <- scenario.list[i.scen]
      print(scenario.name)
      # Their are three source apportionments in the scenarios
      if (scenario.name %in% c("motorway", "nonurban", "urban")) {
        SA.type <- "SA_network"
      } else if (scenario.name %in% c("smallLEZ", "bigLEZ", "Complement")) {
        SA.type <- "SA_zone"
      } else if (scenario.name == "basecase") {
        SA.type <- "basecase"
      } else {
        SA.type <- "SA_fleet"
      }
      
      # read the local NOx netcdf
      local.nox.nc <- local.nox.files[i.scen]
      local.nox.raster <- raster(file.path(city.results.folder, local.nox.nc))
      
      # get the extent and convert to WGS 84
      extent.polygon.utm <- as(extent(local.nox.raster), 'SpatialPolygons')
      proj4string(extent.polygon.utm) <- city.epsg #city.projection
      extent.polygon.wgs84 <- spTransform(extent.polygon.utm, CRS("+proj=longlat +ellps=WGS84"))
      
      # all stations in UTM of the current city
      stations.utm <- spTransform(stations.spdf, city.epsg)
      
      # get stations inside the domain
      station.in.domain <- gIntersects(extent.polygon.wgs84, stations.spdf, byid = TRUE)
      # if (sum(station.in.domain)>0) {}
      city.stations.wgs84.spdf <- SpatialPointsDataFrame(coords = matrix(stations.spdf@coords[station.in.domain,], 
                                                                         ncol=2),
                                                         data = stations.spdf@data[station.in.domain,],
                                                         proj4string = CRS("+proj=longlat +ellps=WGS84"))
      
      city.stations.utm.spdf <- spTransform(city.stations.wgs84.spdf, city.epsg) # city.projection
      n.stations <- NROW(city.stations.utm.spdf)
      city.stations.utm.spdf@data$letter.code <- LETTERS[1:n.stations]
      
      # make this map only once for the basecase
      if (scenario.name == "basecase") {
        # get background NOx
        total.nox.nc <- file.path(city.output.folder, "results", "basecase", "NOx_total_conc.asc")
        total.nox.raster <- raster(total.nox.nc)
        # an expensive way to retrieve the background NOx from the results
        background.nox <- mean(values(total.nox.raster-local.nox.raster), na.rm = T)
        mean.local.nox <- mean(values(local.nox.raster), na.rm = T)

        # read local NO2 raster
        total.no2.nc <- file.path(city.output.folder, "results", "basecase", "NO2_total_conc.asc")
        total.no2.raster <- raster(total.no2.nc)
        median.no2 <- round(median(values(total.no2.raster), na.rm = T))
        
        # breaks and labels
        no2.levels <- round(my.levels / min(10, (35/median.no2)), 1)
        top.level <- no2.levels[n.levels-1]
        no2.labels <- sapply(no2.levels, toString)
        no2.labels[n.levels] <- paste0(">", no2.labels[n.levels-1])
        
        # put all values above the top level to top.level+1 (to have a nicer scale)
        total.no2.raster[total.no2.raster>top.level] <- top.level + 0.001
        
        # open the zones file
        dsn.zones.shp <- file.path(cities.zones.folder, cityname, paste0("/zones_", cityname)) 
        layer.zones.shp <- paste0("zones_", cityname)
        zones.shp <- file.path(dsn.zones.shp, paste0(layer.zones.shp, ".shp"))
        zones.spdf <- readOGR(dsn = dsn.zones.shp, layer = layer.zones.shp)
        zones.utm.spdf <- spTransform(zones.spdf, city.epsg) # city.projection  
        
        station.colors <- sapply(city.stations.utm.spdf@data$AirPollutionLevel, getColor, no2.levels, ircel.colors)
        
        # make a map with the local NO2, zones and stations
        tiff(paste0("_postprocessing/", cityname, "_stations.tiff"), 
             width = 2*480, height = 2*480, res = 144)
        plot(total.no2.raster, 
             breaks = no2.levels,
             col = ircel.colors,
             lab.breaks = no2.labels,
             legend.args=list(text=expression('NO'[2]*' ('*mu*'g/m'^3*')')), # , side=4, font=2, line=2.5, cex=0.8
             bty = 'n', xaxt = 'n', yaxt = 'n',
             main=paste0(cityname)) # alpha = 0.5, "Stations in ", 
        plot(city.utm.sldf, col="grey", add=T)
        plot(zones.utm.spdf, border = "black", add=T)
        # plot(city.stations.utm.spdf, add=T, pch=19)
        text(x = city.stations.utm.spdf@coords, pos = 3,
             labels = city.stations.utm.spdf@data$letter.code)
        # add points with the same color scale for the stations
        points(city.stations.utm.spdf@coords, pch=19, col=station.colors)
        points(city.stations.utm.spdf@coords) # black circles
        dev.off()
        
        # Plot of local NOx
        # ----------------------
        # read local NO2 raster
        mean.nox <- round(mean(values(local.nox.raster)))
        
        # breaks and labels
        nox.levels <- round(my.levels / min(10, 35 / mean.nox), 1)
        top.level <- nox.levels[n.levels-1]
        nox.labels <- sapply(nox.levels, toString)
        nox.labels[n.levels] <- paste0(">", nox.labels[n.levels-1])
        
        # put all values above the top level to top.level+1 (to have a nicer scale)
        local.nox.raster[local.nox.raster>top.level] <- top.level + 0.001
        
        # make a map with the local NO2, zones and stations
        tiff(paste0("_postprocessing/", cityname, "_local_NOx.tiff"), 
             width = 2*480, height = 2*480, res = 144)
        plot(local.nox.raster, 
             breaks = nox.levels,
             col = ircel.colors,
             lab.breaks = nox.labels,
             legend.args=list(text=expression('NO'["x"]*' ('*mu*'g/m'^3*')')), # , side=4, font=2, line=2.5, cex=0.8
             bty = 'n', xaxt = 'n', yaxt = 'n',
             main=paste0(cityname)) # alpha = 0.5, "Stations in ", 
        plot(city.utm.sldf, col="grey", add=T)
        plot(zones.utm.spdf, border = "black", add=T)
        # plot(city.stations.utm.spdf, add=T, pch=19)
        text(x = city.stations.utm.spdf@coords, pos = 3,
             labels = city.stations.utm.spdf@data$letter.code)
        points(city.stations.utm.spdf@coords) # black circles
        dev.off()
      }
      # expression("Scenario"~.(scenario_number)~.(pol_code)[.(pol_sub)]~"["*mu*"g/m"^3*"]")
      
      # extract NOx concentrations for stations
      for (i.station in 1:n.stations) {
        station.utm.coord <- matrix(city.stations.utm.spdf@coords[i.station,], ncol = 2)
        nox.stat.scen <- extract(local.nox.raster, station.utm.coord)
        no2.basecase <- extract(total.no2.raster, station.utm.coord)
        restult.at.station <- data.frame(cityname = cityname,
                                         AQStationName = city.stations.utm.spdf@data$AQStationName[i.station],
                                         StationLetter = city.stations.utm.spdf@data$letter.code[i.station],
                                         Longitude = city.stations.utm.spdf@data$Longitude[i.station],
                                         Latitude = city.stations.utm.spdf@data$Latitude[i.station],
                                         AirPollutionLevel = city.stations.utm.spdf@data$AirPollutionLevel[i.station],
                                         x.utm = station.utm.coord[1],
                                         y.utm = station.utm.coord[2],
                                         scenario.name = scenario.name,
                                         SA.type = SA.type,
                                         NOx = nox.stat.scen,
                                         NO2.mod = no2.basecase)
        results.at.stations.df <- rbind(results.at.stations.df, restult.at.station)
      } # loop over stations
    } # loop over scenarios
    
    # add the background for each SA.type
    for (SA.type in c("basecase", "SA_zone", "SA_fleet", "SA_network")) {
      for (i.station in 1:n.stations) {
        background.at.station <- data.frame(cityname = cityname,
                                         AQStationName = city.stations.utm.spdf@data$AQStationName[i.station],
                                         StationLetter = city.stations.utm.spdf@data$letter.code[i.station],
                                         Longitude = city.stations.utm.spdf@data$Longitude[i.station],
                                         Latitude = city.stations.utm.spdf@data$Latitude[i.station],
                                         AirPollutionLevel = city.stations.utm.spdf@data$AirPollutionLevel[i.station],
                                         x.utm = station.utm.coord[1],
                                         y.utm = station.utm.coord[2],
                                         scenario.name = "background",
                                         SA.type = SA.type,
                                         NOx = background.nox,
                                         NO2.mod = NA)
        results.at.stations.df <- rbind(results.at.stations.df, background.at.station)
      }
    }
    
    
    # sort the data first per source apportionment, station and scenario
    scenario.order <- c("basecase", "motorway", "nonurban", "urban", "smallLEZ", "bigLEZ", "Complement", 
                        "Truck", "Bus", "Van", "DieselCarE03", "DieselCarE4", "DieselCarE5", "DieselCarE6", 
                        "GasolineCar", "OtherCar", "MoMo", "background")
    results.at.stations.df$scenario.name <- factor(results.at.stations.df$scenario.name,
                                                    levels = scenario.order,
                                                    ordered = TRUE)
    results.at.stations.df <- results.at.stations.df[order(results.at.stations.df$SA.type,
                                                           results.at.stations.df$AQStationName, 
                                                           results.at.stations.df$scenario.name),]
    
    write.table(results.at.stations.df,
                file = paste0("_postprocessing/", cityname, "_station_results.csv"),
                row.names = F, quote = F, sep = ",")
    
    # bar plots of NOx per zone, road type and fleet sub-category
    # png(paste0("_postprocessing/", cityname, "_totalNOx_SA_barplots.png"), height = 480, width = 480/3*max(n.stations,3))
    # p <- ggplot(results.at.stations.df[results.at.stations.df$SA.type != "basecase",], 
    #             aes(x=SA.type, y=NOx, fill = scenario.name)) 
    # p <- p + geom_col() + facet_grid(~ AQStationName + StationLetter)
    # p <- p + scale_fill_manual(values=c("basecase"="black", "motorway"="red", "nonurban"="blue", "urban"="green",
    #                                     "smallLEZ"="orange", "bigLEZ"="yellow", "Complement"="purple",
    #                                     "Truck"="black", "Bus"="blue", "Van"="yellow",
    #                                     "DieselCarE03"="firebrick4", "DieselCarE4"="firebrick3", "DieselCarE5"="firebrick2", "DieselCarE6"="darkorange",
    #                                     "GasolineCar"="green", "OtherCar"="blue", "MoMo"="pink", "background"="grey"))
    # print(p)
    # dev.off()

    # # bar plots of NOx per zone, road type and fleet sub-category
    # png(paste0("_postprocessing/", cityname, "_localNOx_SA_barplots.png"), height = 480, width = 480/3*max(n.stations,3))
    # p <- ggplot(results.at.stations.df[!(results.at.stations.df$scenario.name == "background" | results.at.stations.df$SA.type == "basecase"),], 
    #             aes(x=SA.type, y=NOx, fill = scenario.name)) 
    # p <- p + geom_col() + facet_grid(~ AQStationName + StationLetter)
    # p <- p + scale_fill_manual(values=c("basecase"="black", "motorway"="red", "nonurban"="blue", "urban"="green",
    #                                     "smallLEZ"="orange", "bigLEZ"="yellow", "Complement"="purple",
    #                                     "Truck"="black", "Bus"="blue", "Van"="yellow",
    #                                     "DieselCarE03"="firebrick4", "DieselCarE4"="firebrick3", "DieselCarE5"="firebrick2", "DieselCarE6"="darkorange",
    #                                     "GasolineCar"="green", "OtherCar"="blue", "MoMo"="pink", "background"="grey"))
    # print(p)
    # dev.off()
    
    # measurements vs model
    meas.vs.model.df <- results.at.stations.df[results.at.stations.df$scenario.name == "basecase",]
    xy.max <- max(meas.vs.model.df$NO2.mod, meas.vs.model.df$AirPollutionLevel, na.rm = T)
    png(paste0("_postprocessing/", cityname, "_NO2_validation.png"))
    plot(meas.vs.model.df$NO2.mod, meas.vs.model.df$AirPollutionLevel,
         xlim = c(0, xy.max), ylim = c(0, xy.max),
         xlab = "modelled NO2 (ug/m3)", ylab = "Measured NO2 (ug/m3)",
         main = cityname)
    text(meas.vs.model.df$NO2.mod, meas.vs.model.df$AirPollutionLevel,
         meas.vs.model.df$AQStationName, pos=1)
    abline(a=0, b=1, col="red")
    dev.off()
    
    # data in GIS format for Katalin
    gis.results.df <- data.frame()
    # SA.type <- "SA_network"
    # stations.list <- as.vector(unique(results.at.stations.df$AQStationName))
    # for (SA.type in c("basecase", "SA_network", "SA_zone", "SA_fleet")) {
    #  soap.at.station.df <- results.at.stations.df[results.at.stations.df$SA.type==SA.type,]
    # scenario.name <- "basecase"
    for (scenario.name in scenario.order) {
      scenario.at.station.df <- results.at.stations.df[results.at.stations.df$scenario.name==scenario.name,]
      scenario.at.station.df <- scenario.at.station.df[,c(1:6,9)]
      names(scenario.at.station.df)[7] <- paste0(scenario.name, "_NOx_ugm3")
      if (NROW(gis.results.df)==0) {
        gis.results.df <- scenario.at.station.df
      } else {
        gis.results.df <- merge(gis.results.df, scenario.at.station.df)
      }
    }
    # }
    write.table(gis.results.df,
                file = paste0("_postprocessing/", cityname, "_GIS_station_results.csv"),
                row.names = F, quote = F, sep = ",")
    
  } # if there are results for some scenarios
} # loop over cities


  
  
  
  
  
  
  
  
  







