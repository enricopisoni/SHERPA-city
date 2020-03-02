# ---------------------------------------------------------------------
# Postprocessing of the results to assess the impact of Madrid Central
# ---------------------------------------------------------------------

# In this case a source apportionment for Madrid Central of
# the following contributions
# - the background
# - the low emission zone 'Madrid central'
# - the complementary area as a whole
# - 

# set the directory of this script as working directory
rm(list = ls())
# wd <- dirname(sys.frame(1)$ofile)
wd <- "D:/SHERPAcity/Case_study_Madrid/SC_code"
setwd(wd)

library(ggplot2)
library(raster)
library(rgeos)
library(rgdal)
library(plyr)
library(raster)
# library(parallel)
source("long2UTM.R")
source("NO2_atlas_config.R")

# Traffic flows for 'OTM trafficvol', 'OTM capcor', 'AADT JRC'
# ------------------------------------------------------------

#scenario.name <- "Madrid18"
vkm.zone.scenario.df <- data.frame()
for (scenario.name in c("Madrid18", "Madrid19")) {
  grid.tbl.txt <- file.path("AllMadrid", scenario.name, paste0(scenario.name, "_grid_tbl.txt"))
  grid.tbl.df <- read.table(grid.tbl.txt, sep = ";", header = T)
  vkm.zone <- ddply(grid.tbl.df, c("zone_name"), summarise, 
                    scenario.name = scenario.name,
                    vkm.aadt.madrid = sum(length_in_cell * aadtmad) / 1e3,
                    vkm.aadt.trafficvol = sum(length_in_cell * trafficvol) / 1e3,
                    vkm.aadt.capcor = sum(length_in_cell * capcor) / 1e3)
  vkm.zone.scenario.df <- rbind(vkm.zone.scenario.df, vkm.zone)
}
write.table(vkm.zone.scenario.df, file = "vkm_zone_year.csv", sep = ",", row.names = F)

# concentration diff map
# ----------------------

NO2.2018.basecase.asc <- "AllMadrid/Madrid18/results/basecase/NO2_total_conc.asc"
NO2.2018.basecase.r <- raster(NO2.2018.basecase.asc)

NO2.2019.LEZoffical.asc <- "AllMadrid/Madrid19/results/LEZ_official/NO2_total_conc.asc"
NO2.2019.LEZoffical.r <- raster(NO2.2019.LEZoffical.asc)

NO2.diff.2019.2018.r <- NO2.2019.LEZoffical.r - NO2.2018.basecase.r
plot(NO2.diff.2019.2018.r)
writeRaster(NO2.diff.2019.2018.r, filename = "AllMadrid/Madrid19/results/LEZ_official/diff_NO2_2019_2018.asc",
            overwrite = T)

NO2.2018.LEZofficial.asc <- "AllMadrid/Madrid18/results/LEZ_official/NO2_total_conc.asc"
NO2.2018.LEZofficial.r <- raster(NO2.2018.LEZofficial.asc)

NO2.diff.2018.LEZ.base.r <- NO2.2018.LEZofficial.r - NO2.2018.basecase.r
plot(NO2.diff.2018.LEZ.base.r)
writeRaster(NO2.diff.2018.LEZ.base.r, filename = "AllMadrid/Madrid18/results/LEZ_official/diff_NO2_2018_LEZ_base.asc",
            overwrite = T)

# Nudge factor between EMEP basecase and EMEP without SNAP7
# ---------------------------------------------------------

NOx.EMEP.basecase.asc <- "AllMadrid/Madrid18/results/Madrid_notraffic_EMEPstand/NOx_total_conc.asc"
NOx.EMEP.basecase.r <- raster(NOx.EMEP.basecase.asc)

NOx.EMEP.SNAP7zero.asc <- "AllMadrid/Madrid18/results/Madrid_notraffic/NOx_total_conc.asc"
NOx.EMEP.SNAP7zero.r <- raster(NOx.EMEP.SNAP7zero.asc)

# the local contribution
NOx.local.basecase.asc <- "AllMadrid/Madrid18/results/basecase/NOx_total_conc.asc"
NOx.local.basecase.r <- raster(NOx.local.basecase.asc)

NOx.traffic.EMEP <- mean(values(NOx.EMEP.basecase.r)) - mean(values(NOx.EMEP.SNAP7zero.r))
NOx.traffic.local <- mean(values(NOx.local.basecase.r))

nudging.factor <- NOx.traffic.EMEP / NOx.traffic.local


# function to calculate the average concentration in the LEZ for
# each scenario of a city
soap4lez <- function(cityname) {
  library(ggplot2)
  library(raster)
  library(rgeos)
  library(rgdal)
  # library(parallel)
  source("long2UTM.R")
  source("NO2_atlas_config.R")
  
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
    # scenario_name <- "basecase" # for testing
    res.df <- data.frame()
    for (scenario_name in scenario.list) {
      print(scenario_name)
      # local contribution and total concentration raster files
      local.conc.asc <- file.path(city.output.folder, "results", scenario_name, paste0(pollutant, "_local_conc.asc"))
      total.conc.asc <- file.path(city.output.folder, "results", scenario_name, paste0(pollutant, "_total_conc.asc"))
      no2.conc.asc <- file.path(city.output.folder, "results", scenario_name, paste0("NO2_total_conc.asc"))
      
      if (file.exists(local.conc.asc) & file.exists(total.conc.asc)) {
        # read the raster file of the local concentration and convert it into a points dataframe
        local.conc.r <- raster(local.conc.asc)
        proj4string(local.conc.r) <- network.proj4string
        local.conc.points <- rasterToPoints(local.conc.r)
        local.conc.spdf <- SpatialPointsDataFrame(coords = local.conc.points[,c("x", "y")],
                                                  data = data.frame(local.conc = local.conc.points[,"NOx_local_conc"]),
                                                  proj4string = CRS(network.proj4string))
        # select points inside the LEZ
        points.in.LEZ.spdf <- intersect(local.conc.spdf, zones.utm.spdf)
        names(points.in.LEZ.spdf@data)[1] <- "local.conc"
        # calculate the mean concentration
        mean.local.conc.LEZ <- mean(points.in.LEZ.spdf$local.conc)
        
        # read the raster file of the total concentration and convert it into a points dataframe
        total.conc.r <- raster(total.conc.asc)
        proj4string(total.conc.r) <- network.proj4string
        total.conc.points <- rasterToPoints(total.conc.r)
        total.conc.spdf <- SpatialPointsDataFrame(coords = local.conc.points[,c("x", "y")],
                                                  data = data.frame(total.conc = total.conc.points[,"NOx_total_conc"]),
                                                  proj4string = CRS(network.proj4string))
        # select points inside the LEZ
        points.in.LEZ.spdf <- intersect(total.conc.spdf, zones.utm.spdf)
        # calculate the mean concentration
        mean.total.conc.LEZ <- mean(points.in.LEZ.spdf$total.conc)
        
        # read the raster file of the NO2 concentration and convert it into a points dataframe
        no2.conc.r <- raster(no2.conc.asc)
        proj4string(no2.conc.r) <- network.proj4string
        no2.conc.points <- rasterToPoints(no2.conc.r)
        no2.conc.spdf <- SpatialPointsDataFrame(coords = local.conc.points[,c("x", "y")],
                                                data = data.frame(no2.conc = no2.conc.points[,"NO2_total_conc"]),
                                                proj4string = CRS(network.proj4string))
        # select points inside the LEZ
        points.in.LEZ.spdf <- intersect(no2.conc.spdf, zones.utm.spdf)
        # calculate the mean concentration
        mean.no2.conc.LEZ <- mean(points.in.LEZ.spdf$no2.conc)
        
        
        # overall mean local and total concentration
        mean.total.conc <- mean(values(total.conc.r))
        mean.local.conc <- mean(values(local.conc.r))
        mean.no2.conc <- mean(values(no2.conc.r))
        
        # impact outside the LEZ
        total.conc.df <- cbind(total.conc.spdf@coords, total.conc.spdf@data) 
        points.in.LEZ.df <- data.frame(points.in.LEZ.spdf@coords, in.LEZ = 1)
        points.outside.LEZ.df <- merge(total.conc.df, points.in.LEZ.df, by=c("x","y"), all=T)
        mean.total.conc.comp <- mean(points.outside.LEZ.df$total.conc[is.na(points.outside.LEZ.df$in.LEZ)])
        
        local.conc.df <- cbind(local.conc.spdf@coords, local.conc.spdf@data) 
        points.outside.LEZ.df <- merge(local.conc.df, points.in.LEZ.df, by=c("x","y"), all=T)
        mean.local.conc.comp <- mean(points.outside.LEZ.df$local.conc[is.na(points.outside.LEZ.df$in.LEZ)])
        
        no2.conc.df <- cbind(no2.conc.spdf@coords, no2.conc.spdf@data) 
        points.outside.LEZ.df <- merge(no2.conc.df, points.in.LEZ.df, by=c("x","y"), all=T)
        mean.no2.conc.comp <- mean(points.outside.LEZ.df$no2.conc[is.na(points.outside.LEZ.df$in.LEZ)])
        pct90.no2.conc.comp <- quantile(points.outside.LEZ.df$no2.conc[is.na(points.outside.LEZ.df$in.LEZ)], prob = 0.99)
        
        # resulta summary
        res.df <- rbind(res.df,
                        data.frame(cityname = cityname,
                             scenario.name = scenario_name,
                             zone = c("LEZ", "Complementary", "Domain"),
                             mean.total.NOx = c(mean.total.conc.LEZ, mean.total.conc.comp, mean.total.conc),
                             mean.local.NOx = c(mean.local.conc.LEZ, mean.local.conc.comp, mean.local.conc),
                             mean.NO2 = c(mean.no2.conc.LEZ, mean.no2.conc.comp, mean.no2.conc)))
        
        
        
        # # if basecase read also total concentration and substract local to have the impact of the background
        # if (scenario_name == "basecase") {
        #   total.conc.asc <- file.path(city.output.folder, "results", scenario_name, paste0(pollutant, "_total_conc.asc"))
        #   # read the raster file of the local concentration and convert it into a points dataframe
        #   total.conc.r <- raster(total.conc.asc)
        #   proj4string(total.conc.r) <- network.proj4string
        #   total.conc.points <- rasterToPoints(total.conc.r)
        #   total.conc.spdf <- SpatialPointsDataFrame(coords = total.conc.points[,c("x", "y")],
        #                                             data = as.data.frame(total.conc.points[, paste0(pollutant, "_total_conc")]),
        #                                             proj4string = CRS(network.proj4string))
        #   # select points inside the LEZ
        #   points.in.LEZ.spdf <- intersect(total.conc.spdf, zones.utm.spdf)
        #   names(points.in.LEZ.spdf@data)[1] <- "total.conc"
        #   # calculate the mean concentration
        #   mean.total.conc.LEZ <- mean(points.in.LEZ.spdf$total.conc)
        #   mean.background.conc.LEZ <- mean.total.conc.LEZ - mean.local.conc.LEZ
        #   
        #   # store background result in data.frame
        #   scen.results.df <- data.frame(source.area = "World",
        #                                 source.fleet = "AllButLocalTraffic",
        #                                 conc.area = "LEZ",
        #                                 pollutant = pollutant,
        #                                 conc = mean.background.conc.LEZ)
        #   soap.results.df <- rbind(soap.results.df, scen.results.df)
        #   
        # } else {
        #   # store background result in data.frame
        #   if (grepl("SAzone_", scenario_name) == TRUE) {
        #     source.fleet <- "FullFleet"
        #     source.area <- gsub("SAzone_", "", scenario_name)
        #   } else if (grepl("SAfleet_", scenario_name) == TRUE) {
        #     source.fleet <- gsub("SAfleet_", "", scenario_name)
        #     source.area <- "LEZ"
        #   }
        #   scen.results.df <- data.frame(source.area = source.area,
        #                                 source.fleet = source.fleet,
        #                                 conc.area = "LEZ",
        #                                 pollutant = pollutant,
        #                                 conc = mean.local.conc.LEZ)
        #   soap.results.df <- rbind(soap.results.df, scen.results.df)
        # }
      } # if scenario exists
    } # scenario loop 
      
    # # write result to a table
    # if (!(dir.exists("_source_apportionment"))) {dir.create("_source_apportionment")}
    # write.table(soap.results.df, file.path("_source_apportionment", paste0("soap_", cityname, ".csv")), 
    #             sep = ",", row.names = F)
      
   # scenario definition ok
  } else { # scenario definition ok
    print(paste0("No scenarios defined for ", cityname))
  } # scenario definition not ok
  return(res.df)
} # end function  

res.df <- data.frame()
for (cityname in city.df$cityname) { # city.df$cityname
  res.df <- rbind(res.df, soap4lez(cityname))
}

# write result to a table
if (!(dir.exists("_source_apportionment"))) {dir.create("_source_apportionment")}
write.table(res.df, "Madrid_Central_results.csv",
            sep = ",", row.names = F)

# city.list <- as.vector(city.df$cityname)
# no_cores <- detectCores()
# # Initiate cluster. Never take more cores than scenarios and one less than available on the machine
# cl <- makeCluster(min(no_cores - 1, n.scenarios))
# # add common variables for all scenarios to the cluster environment
# # clusterExport(cl, c("gridded.network.df", "scenario.efs.df", "results.folder",
# #                     "AADT.field", "emission.raster.folder"))
# # throw the runs on the cluster
# parLapply(cl, city.list, soap4lez)
# # stop the cluster
# stopCluster(cl)


for (cityname in c("Luxembourg")) { # city.df$cityname
  # city coordinates 
  city.coords <- matrix(c(city.df$lon[city.df$cityname == cityname],
                          city.df$lat[city.df$cityname == cityname]), ncol = 2,
                          dimnames = list(c(cityname), c("lon", "lat")))
  city.utm <- long2UTM(city.coords[1])
    
  # Without nudging
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
