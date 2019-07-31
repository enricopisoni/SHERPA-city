# --------------------------------------------------------------
# Source apportionemnt of the average concentration over the LEZ
# --------------------------------------------------------------

# In this case a source apportionment for the bigLEZ+smallLEZ for
# the following contributions
# - the background
# - the complementary area as a whole
# - several fleet contributions inside the complete LEZ

wd <- "D:/SHERPAcity/NO2_atlas/run20190724"
setwd(wd)

library(ggplot2)
library(raster)
library(rgeos)
library(rgdal)
source("long2UTM.R")
source("NO2_atlas_config.R")

# function to calculate the average concentration in the LEZ (big+small) for
# each scenario of a city
soap4lez <- function(cityname) {
  # city coordinates 
  city.coords <- matrix(c(city.df$lon[city.df$cityname == cityname],
                          city.df$lat[city.df$cityname == cityname]), ncol = 2,
                        dimnames = list(c(cityname), c("lon", "lat")))
  
  # city output folder and other info
  city.output.folder <- sub("^/", "", file.path(cities.output.folder, cityname))
  scenario.definition.file <- file.path(city.output.folder, paste0(cityname, "_scenario_definition.csv"))
  
  # read the zone shape file for the city and convert it into local UTM
  dsn.zones.shp <- file.path(cities.zones.folder, cityname, paste0("zones_", cityname))
  layer.zones.shp <- paste0("zones_", cityname)
  zones.shp <- paste0(dsn.zones.shp, "/", layer.zones.shp, ".shp")
  # read the zones shape file as a spatialPolygonsDataFrame
  zones.wgs84.spdf <- readOGR(dsn = dsn.zones.shp, layer = layer.zones.shp)
  # convert to local UTM
  network.proj4string <- long2UTM(city.coords[1])
  zones.utm.spdf <- spTransform(zones.wgs84.spdf, network.proj4string)


  # check if there's a scenario definition file and if all scenarios are available
  scen.def.ok <- file.exists(scenario.definition.file)
  if (scen.def.ok == TRUE) {
    # check if all scenarios are available (NOx_conc file)
    # open the scenario definition file
    scenarios.df <- read.table(scenario.definition.file, sep = ",", header = TRUE)
    scenario.list <- as.vector(unique(scenarios.df$scenario_name))
    
    # results data.frame
    soap.results.df <- data.frame()
    scenario_name <- "basecase"
    for (scenario_name in scenario.list) {
      print(scenario_name)
      local.conc.asc <- file.path(city.output.folder, "results", scenario_name, paste0(pollutant, "_local_conc.asc"))
      if (file.exists(local.conc.asc)) {
        # read the raster file of the local concentration and convert it into a points dataframe
        local.conc.r <- raster(local.conc.asc)
        proj4string(local.conc.r) <- network.proj4string
        local.conc.points <- rasterToPoints(local.conc.r)
        local.conc.spdf <- SpatialPointsDataFrame(coords = local.conc.points[,c("x", "y")],
                                                  data = as.data.frame(local.conc.points[, paste0(pollutant, "_local_conc")]),
                                                  proj4string = CRS(network.proj4string))
        # select points inside the LEZ
        points.in.LEZ.spdf <- intersect(local.conc.spdf, zones.utm.spdf)
        names(points.in.LEZ.spdf@data)[1] <- "local.conc"
        # calculate the mean concentration
        mean.local.conc.LEZ <- mean(points.in.LEZ.spdf$local.conc)
        
        # if basecase read also total concentration and substract local to have the impact of the background
        if (scenario_name == "basecase") {
          total.conc.asc <- file.path(city.output.folder, "results", scenario_name, paste0(pollutant, "_total_conc.asc"))
          # read the raster file of the local concentration and convert it into a points dataframe
          total.conc.r <- raster(total.conc.asc)
          proj4string(total.conc.r) <- network.proj4string
          total.conc.points <- rasterToPoints(total.conc.r)
          total.conc.spdf <- SpatialPointsDataFrame(coords = total.conc.points[,c("x", "y")],
                                                    data = as.data.frame(total.conc.points[, paste0(pollutant, "_total_conc")]),
                                                    proj4string = CRS(network.proj4string))
          # select points inside the LEZ
          points.in.LEZ.spdf <- intersect(total.conc.spdf, zones.utm.spdf)
          names(points.in.LEZ.spdf@data)[1] <- "total.conc"
          # calculate the mean concentration
          mean.total.conc.LEZ <- mean(points.in.LEZ.spdf$total.conc)
          mean.background.conc.LEZ <- mean.total.conc.LEZ - mean.local.conc.LEZ
          
          # store background result in data.frame
          scen.results.df <- data.frame(source.area = "World",
                                        source.fleet = "AllButLocalTraffic",
                                        conc.area = "LEZ",
                                        pollutant = pollutant,
                                        conc = mean.background.conc.LEZ)
          soap.results.df <- rbind(soap.results.df, scen.results.df)
          
        } else {
          # store background result in data.frame
          if (grepl("SAzone_", scenario_name) == TRUE) {
            source.fleet <- "FullFleet"
            source.area <- gsub("SAzone_", "", scenario_name)
          } else if (grepl("SAfleet_", scenario_name) == TRUE) {
            source.fleet <- gsub("SAfleet_", "", scenario_name)
            source.area <- "LEZ"
          }
          scen.results.df <- data.frame(source.area = source.area,
                                        source.fleet = source.fleet,
                                        conc.area = "LEZ",
                                        pollutant = pollutant,
                                        conc = mean.local.conc.LEZ)
          soap.results.df <- rbind(soap.results.df, scen.results.df)
        }
      } # if scenario exists
    } # scenario loop 
      
    # write result to a table
    write.table(soap.results.df, file.path("_source_apportionment", paste0("soap_", cityname, ".csv")), 
                sep = ",", row.names = F)
      
   # scenario definition ok
  } else { # scenario definition ok
    print(paste0("No scenarios defined for ", cityname))
  } # scenario definition not ok
}  

for (cityname in city.df$cityname) {
  soap4lez(cityname)
}

for (cityname in city.df$cityname) {
  # read result table
  soap.city.df <- read.table(file.path("_source_apportionment", paste0("soap_", cityname, ".csv")),
                             sep = ",", header = T)
  # offsets for each source area
  World.offset <- 0
  Comp.offset <- soap.city.df$conc[soap.city.df$source.area == "World"]
  LEZ.offset <- Comp.offset + soap.city.df$conc[soap.city.df$source.area == "Complement"]
  offset.df <- data.frame(source.area = c("World", "Complement", "LEZ"),
                          source.fleet = rep("offset", 3),
                          conc.area = rep("LEZ", 3),
                          pollutant = rep("NOx", 3),
                          conc = c(World.offset, Comp.offset, LEZ.offset))
  # add the offsets to the dataframe
  soap.city.df <- rbind(soap.city.df, offset.df)
  
  # create ordered factors
  fleet.levels <- rev(c("offset", "AllButLocalTraffic", "FullFleet", "TruckE05", "TruckE6", "Bus", "Van",
                    "DieselCarE03", "DieselCarE4", "DieselCarE5", "DieselCarE6", "GasolineCar",
                    "OtherCar", "MoMo"))
  fleet.labels <- fleet.levels
  fleet.labels[14] <- ""
  fleet.colors <- rev(c(NA, "grey", "red", "orange4", "orange3", "olivedrab3", "orange",
                        "deepskyblue4",  "deepskyblue2","cyan3","cyan1",
                        "magenta4", "orchid3", "gold1"))
  soap.city.df$source.area <- factor(soap.city.df$source.area, levels = rev(c("World", "Complement", "LEZ")), ordered = T)
  soap.city.df$source.fleet <- factor(soap.city.df$source.fleet, levels = fleet.levels, ordered = T)
  
  p <- ggplot(data = soap.city.df, aes(x=source.area, y=conc, fill=source.fleet))
  p <- p + geom_col() + coord_flip()
  p <- p + scale_colour_manual(values = fleet.colors, 
                               aesthetics = c("colour", "fill"),
                               labels = fleet.labels)
  p <- p + labs(y="NOx [ug/m3]", x="NOx source area", title = paste0("Source apportionment of the ", cityname, " LEZ"))
  p <- p + theme(text = element_text(size=20))
  png(file.path("_source_apportionment", paste0("SOAPbarplot_", cityname, ".png")),
      width = 2*480, height = 480)
  print(p)
  dev.off()
}
