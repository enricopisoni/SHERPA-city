# -------------------#
# NO2 atlas workflow #
# -------------------#

# The workflow consists of the following steps
# 1) Looking up which OTM shape files are needed to construct a shape file of the domain
#    and producue the shape file. By default the city centre (defined in city.df) is taken
#    as the centre of a 20x20 km square. If the lon.min, lon.max, lat.min, and lat.max variables
#    are avaliable in city.df these are taken as domain boundaries. All results of a city are in
#    a subfolder <cityname>/
# 2) Add a column with the zone to the network shp. A zone shape file has to be availalble
#    in a sub-folder <cityname>/zones_<city_name>. The format of this file is strictly defined:
#    - projection WGS84, 3 fields, id (int), name (string), descr (string)
#    - non overlapping polygons named bigLEZ and smallLEZ (for the Atlas)
# 3) Grid the road network (currently in Python)
# 4) Calculate fleet emission factors for all scenarios and zones
# 5) Calculate gridded emissions for each scenario. Scenarios are under <cityname>/results/<scenario_name>
# 6) Apply the dispersion kernels on the emissions

# Clean up
rm(list=ls())
# set the directory of this script as working directory
wd <- dirname(sys.frame(1)$ofile)
setwd(wd)

# load libraries and auxiliary functions
library(raster)
library(geosphere)
library(parallel)
source("NO2_atlas_config.R")
source("create_default_extent.R")          # step 1
source("create_network_shp_utm.R")         # step 1
source("add_zones.R")                      # step 2
source("check_zones_overlap.R")            # step 2
source("create_fleet_emission_factors.R")  # step 4
source("create_gridded_emissions.R")       # step 5
source("sherpacity_par.R")                 # step 6

# loop over all the cities
for (cityname in as.vector(city.df$cityname)) { # as.vector(city.df$cityname)
  print("########################################")
  print(cityname)
  # city coordinates 
  city.coords <- matrix(c(city.df$lon[city.df$cityname == cityname],
                          city.df$lat[city.df$cityname == cityname]), ncol = 2,
                        dimnames = list(c(cityname), c("lon", "lat")))
  
  # create a folder for a specific city in the cities.output.folder if it doesn't exist yet.
  # A leading '/' has to be removed when cities.output.folder = ""
  city.output.folder <- sub("^/", "", file.path(cities.output.folder, cityname))
  if (!dir.exists(city.output.folder)) { 
    dir.create(city.output.folder, recursive = T) 
  }
  
  # folder and filename of the UTM shape file with the network (maybe it exists, maybe not)
  dsn.city.utm.shp <- city.output.folder
  layer.city.utm.shp <- paste0("traffic_roadlinks_", cityname)
  city.utm.shp <- paste0(dsn.city.utm.shp, "/", layer.city.utm.shp, ".shp")

  # to avoid wrong behaviour with previous loop, remove the SpatialLinesDataFrame of the network
  if ("city.utm.sldf" %in% ls()) { rm(city.utm.sldf) }
  
  # folder and filename of the UTM shape file with the network and zones (maybe it exists, maybe not)
  dsn.zoned.city.utm.shp <- city.output.folder
  layer.city.zoned.utm.shp <- paste0("traffic_roadlinks_zones_", cityname)
  zoned.city.utm.shp <- paste0(dsn.zoned.city.utm.shp, "/", layer.city.zoned.utm.shp, ".shp")
  
  # to avoid wrong behaviour with previous loop, remove the SpatialLinesDataFrame of the network
  if ("zoned.city.utm.sldf" %in% ls()) { rm(zoned.city.utm.sldf) }
  
  
  # Step 1: Create the network shape file for the domain
  # ----------------------------------------------------

  # create network shape file inside domain in utm 
  # 1) look up which shape files to use
  # 2) paste them together
  # 3) convert to UTM
  # 4) write a shape file

  if (!file.exists(city.utm.shp) | rerun.domain) {
    print(paste0("Creating a shape file in UTM for ", cityname, " ..."))
    
    # check if domain is defined, if not create one of 20x20km
    lon.lat.limits <- city.df[city.df$cityname == cityname, c('lon.min', 'lon.max', 'lat.min', 'lat.max')]
    # horror, extent doesn't take a df with named columns
    lon.lat.limits <- c(lon.lat.limits[1,1], lon.lat.limits[1,2], lon.lat.limits[1,3], lon.lat.limits[1,4])
    if (is.na(sum(lon.lat.limits))) {
      city.extent <- create.default.extent(city.coords)
    } else {
      # define city extent in latitude longitude (WGS84, epsg: 4326)
      city.extent <- extent(lon.lat.limits)
    }
    
    # create a shape file for the domain, combining the necessary OTM shape files
    city.utm.sldf <- create.network.shp.utm(city.extent, cityname, nuts3.bbox.df, otm.path)
    
    # write the UTM shape file of the network
    writeOGR(city.utm.sldf, dsn = dsn.city.utm.shp, layer = layer.city.utm.shp,
             driver="ESRI Shapefile")
  } else {
    print(paste0("A network shape file in UTM without zones was already created for ", cityname))
  }

  # Step 2: Add a column with the zone to the network shp
  # -----------------------------------------------------
  
  # Add a column to the network shape in utm with the zones
  # the zones have be in a shape file with name <cityname>/zones_<cityname>.shp
  # the attribute table has 2 fields: 'id' and 'zone_name'
  # If no zones area available just add a column 'Compelent'
  
  # check if there is a network shape files with zones added
  if (!file.exists(zoned.city.utm.shp) | rerun.zoning) {
    
    # Check if the network without zones is already in memory from step 1 (city.utm.sldf), if not read it.
    # This avoids reading the shp file twice (which is slow) but it is important that before
    # starting a new city the varialbes 'city.utm.sldf' and 'zoned.city.utm.sldf' are removed.
    # dangerous when looping over cities
    if (!("city.utm.sldf" %in% ls())) {
      # if the network shape file already exists, read it
      print(paste0("Reading ", dsn.city.utm.shp, "/", layer.city.utm.shp, ".shp"))
      city.utm.sldf <- readOGR(dsn = dsn.city.utm.shp, layer = layer.city.utm.shp)
    }
    
    # Check if there is a shape file with zones. If so, add the zones to the network shape file.
    dsn.zones.shp <- paste0(city.output.folder, "/zones_", cityname) 
    layer.zones.shp <- paste0("zones_", cityname)
    zones.shp <- paste0(dsn.zones.shp, "/", layer.zones.shp, ".shp")
    if (file.exists(zones.shp)) {
      # read the zones shape file as a spatialPolygonsDataFrame
      zones.spdf <- readOGR(dsn = dsn.zones.shp, layer = layer.zones.shp)
      
      # check if the zones are not overlapping
      overlap <- check.zones.overlap(zones.spdf)
      if (overlap == TRUE) {
        print("Overlapping zones. Modify the zones file.")
      } else {
        # Add a column with the zone to each road 
        print(paste0("Adding zones to network to create ", zoned.city.utm.shp))
        zoned.city.utm.sldf <- add.zones(city.utm.sldf, zones.spdf, cityname)
      }
  
    # if there is no zones_<cityname>.shp just add a column for the complementary zone
    } else {
      print(paste0("No zones for ", cityname, ". Put a zones_<cityname>.shp in the zones_<cityname> folder."))
      # print(paste0("No zones for ", cityname, ". A default complementary zone was added."))
      # zoned.network.shp.utm <- network.shp.utm
      # zoned.network.shp.utm@data$zone_name <- "Complement"
    }
    
    # write the UTM shape file with zone(s) if it was produced. There are 2 possibilities:
    # 1) A valid zones file was provided (without overlapping zones)
    # 2) No zones file, just one complementary zone everywhere
    if ("zoned.city.utm.sldf" %in% ls()) {
      writeOGR(zoned.city.utm.sldf, 
         dsn = dsn.zoned.city.utm.shp, 
         layer = layer.city.zoned.utm.shp,
         driver = "ESRI Shapefile",
         overwrite = TRUE)
      # !!!!!!!!!!!
      # to save some space the network shape file without zones could be deleted
      # !!!!!!!!!!!
    }

  } else {
    print(paste0("A zoned network shape file already exists for ", cityname))
  } # close if for zoned network shape
  
  # remove zoned.network.shp.utm to avoid wrong behaviour in next loop. Just to be sure.
  for (myVar in c('zoned.network.shp.utm', 'network.shp.utm', 'zones.shp')) {
    if (myVar %in% ls()) {rm(myVar)}
  }
  
  # Step 3: Grid the road network (currently in Python)
  # ---------------------------------------------------
  
  # see fast_gridding.py
  gridded.network.file <- paste0(city.output.folder, "/", cityname, "_grid_tbl.txt")
  
  # Step 4: Calculate fleet emission factors
  # ----------------------------------------
  
  # scenario definition file
  # 5 columns: scenario_name,zone_name,default_fleet_country,default_fleet_year,fleet_configuration
  scenario.definition.file <- paste0(city.output.folder, "/", cityname, "_scenario_definition.csv")
  # emission factor file
  emission.factors.file <- paste0(city.output.folder, "/", cityname, "_emission_factors.csv")
  
  if (file.exists(scenario.definition.file)) {
    if (!file.exists(emission.factors.file)) {
      # create a data frame with the emission factors to be used per
      # scenario, zone and road type
      scenario.efs.df <- create_fleet_emission_factors(scenario.definition.file)
      write.table(scenario.efs.df, file = emission.factors.file, sep = ",", row.names = FALSE, quote = FALSE)
    } else {
      print(paste0("Emission factors are already calcultated for ", cityname))
    }
  } else {
    print(paste0("No scenarios available for ", cityname))
  }
  
  # Step 5: Create emission rasters for scenarios
  # ---------------------------------------------
  
  # check if a gridded network table and emission factors are available
  if (file.exists(gridded.network.file) & file.exists(emission.factors.file)) {
    print(paste0("Creating emission rasters for ", cityname))
    
    # read the emission factors
    scenario.efs.df <- read.table(emission.factors.file, sep = ",", header = TRUE)
    scenario.list <- as.vector(unique(scenario.efs.df$scenario_name))
    # read the gridded network
    gridded.network.df <- read.table(gridded.network.file, sep = ";", header = TRUE, quote = "")

    # create results folder if it doesn't exist
    results.folder <- file.path(city.output.folder, "results")
    if (!dir.exists(results.folder)) { dir.create(results.folder) }
    
    # Calculate the number of cores
    no_cores <- detectCores()
    # Initiate cluster
    cl <- makeCluster(no_cores-1) # no_cores
    # add common variables for all scenarios to the cluster environment
    clusterExport(cl, c("gridded.network.df", "scenario.efs.df", "results.folder", "AADT.field"))
    # throw the runs on the cluster
    parLapply(cl, scenario.list, create.gridded.emissions)
    # stop the cluster
    stopCluster(cl)
    
    # # loop over all scenarios (seqential code)
    # for (scenario_name in scenario.list) {
    #   # create results folder if it doesn't exist
    #   results.folder <- paste0(cityname, "/results/")
    #   if (!dir.exists(results.folder)) { dir.create(results.folder) }
    #   
    #   # create a scenario folder if it doesn't exitst yet
    #   scenario.folder <- paste0(results.folder, scenario_name)
    #   if (!dir.exists(scenario.folder)) { dir.create(scenario.folder) }
    #   
    #   # create gridded emissions for each scenario
    #   # output will be written to the scenario folder (emis_NOx.asc, emis_PM25.asc)
    #   # add if exist
    #   if (!file.exists(paste0(scenario.folder, "/emis_NOx.asc"))) {
    #     print(paste("Running scenario", scenario_name, "in", cityname))
    #     create.gridded.emissions(scenario_name, gridded.network.df, scenario.efs.df, scenario.folder)
    #   } else {
    #     print(paste("Scenario", scenario_name, "in", cityname, "already exists."))
    #   }
    # }

  } else {
    if(!(file.exists(gridded.network.file))) {
      print(paste0("No gridded network available for ", cityname, ". Run the python script."))
    }
    if(!(file.exists(scenario.definition.file))) {
      print(paste0("No scenario definitions for ", cityname))
    }
  }

  
  # Step 6: Calculate concentrations for the scenarios
  # --------------------------------------------------
  
  # the variables 'pollutant' and 'sc.config.file' are defined in the
  # config file 'NO2_atlas_config.R'.

  if (file.exists(gridded.network.file) & file.exists(emission.factors.file)) {
    print(paste0("Creating concentration rasters for ", cityname))
          
    input.list <- list()
    n.scenarios <- length(scenario.list)
    # loop over all scenarios
    for (i in 1:n.scenarios) {
      input.list[[i]] <- list("sc.config.file" = sc.config.file,
                              "city.coords" = city.coords,
                              "emis.raster.file" = file.path(results.folder, scenario.list[i], paste0("emis_", pollutant, ".asc")),
                              "pollutant" = if(pollutant=="NOx") {"NO2"} else {pollutant},
                              "output.path" = file.path(results.folder, scenario.list[i]))
    }
    # for testing
    # sherpacity_par(input.list[[1]])
    
    # Calculate the number of cores
    no_cores <- detectCores()
    # Initiate cluster with as many cores as scenarios if possible, but never more than the
    # total number of cores minus 1
    cl <- makeCluster(min(no_cores-1, n.scenarios))
    # throw the runs on the cluster
    parLapply(cl, input.list, sherpacity_par)
    # stop the cluster
    stopCluster(cl)

    # # loop over all scenarios (sequential)
    # for (scenario_name in scenario.list) {
    #   # check if the emission raster exists
    #   pollutant <- "NOx"
    #   for (pollutant in c("NOx")) { # , "PM25"
    #     output.path <- paste0(cityname, "/results/", scenario_name, "/")
    #     emis.raster.file <- paste0(output.path, "emis_", pollutant, ".asc")
    #     if (file.exists(emis.raster.file)) {
    #       conc.file <- paste0(output.path, pollutant, "_total_conc.asc")
    #       if (!file.exists(conc.file)) {
    #         # apply kernel approach to emissions
    #         print(paste0("Calculation of ", pollutant, " concentrations in ", cityname, " for the ", scenario_name, " scenario."))
    #         sherpacity(sc.config.file, city.coords, emis.raster.file,
    #                    if (pollutant == "NOx") {"NO2"} else {pollutant}, output.path)
    #       } else {
    #         print(paste0(pollutant, " concentrations are already calculated for ", scenario_name, " in ", cityname))
    #       }
    #     } else {
    #       print(paste0("No emissions raster for ", pollutant, " in the ", scenario_name, " scenario in ", cityname))
    #     }
    #   }
    # }  
  }
  

} # close loop over cities



