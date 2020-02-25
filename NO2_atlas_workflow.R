#--------------------#
# NO2 atlas workflow #
#--------------------#

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
# wd <- dirname(sys.frame(1)$ofile)
wd <- "D:/SHERPAcity/Case_study_Madrid/SC_code"
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
source("create_fleet_configurations.R")    # step 4
source("create_scenario_definition.R")     # step 4
source("create_fleet_emission_factors.R")  # step 4
source("create_gridded_emissions.R")       # step 5
source("sherpacity_par.R")                 # step 6
source("long2UTM.R")


# Create fleet configurations
# ---------------------------
if (file.exists(file.path(fleet.configuration.folder, fleet.config.overview.file))) {
  create.fleet.configs(fleet.configuration.folder, fleet.config.overview.file)
} else {
  print(paste0("No fleet configurations could be produced because the overview file ",
               fleet.config.overview.file, " was not found."))
}

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
  
  # create results folder if it doesn't exist
  results.folder <- file.path(city.output.folder, "results")
  if (!dir.exists(results.folder)) { dir.create(results.folder) }
  
  # folder and filename of the UTM shape file with the network (maybe it exists, maybe not)
  dsn.city.utm.shp <- file.path(cities.network.folder, cityname)
  layer.city.utm.shp <- paste0("traffic_roadlinks_", cityname)
  city.utm.shp <- file.path(dsn.city.utm.shp, paste0(layer.city.utm.shp, ".shp"))

  # to avoid wrong behaviour with previous loop, remove the SpatialLinesDataFrame of the network
  if ("city.utm.sldf" %in% ls()) { rm(city.utm.sldf) }
  
  # folder and filename of the UTM shape file with the network and zones (maybe it exists, maybe not)
  dsn.zoned.city.utm.shp <- file.path(cities.network.folder, cityname)
  layer.city.zoned.utm.shp <- paste0("traffic_roadlinks_zones_", cityname)
  zoned.city.utm.shp <- file.path(dsn.zoned.city.utm.shp, paste0(layer.city.zoned.utm.shp, ".shp"))
  
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
    print(paste0("A network shape file in UTM without zones already exists for ", cityname))
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
    dsn.zones.shp <- file.path(cities.zones.folder, cityname, paste0("zones_", cityname))
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
    
    # Step 2.1: Add the traffic volumes with the data from Panos in a new field 'aadt_madrid'
    # ---------------------------------------------------------------------------------------
    
    
    
    
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
         # file.remove(file.path(dsn.city.utm.shp, paste0(layer.city.utm.shp, c(".shp", ".dbf", ".prj", ".shx"))))
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
  gridded.network.file <- file.path(cities.network.folder, cityname, paste0(cityname, "_grid_tbl.txt"))
  
  # Step 4: Calculate fleet emission factors for all scenarios
  # ----------------------------------------------------------
  
  # Create scenario definition file from a template
  create.scenario.definition(scenario.template.file, city.df, cityname, fleet.year, city.output.folder)
  # 5 columns: scenario_name,zone_name,default_fleet_country,default_fleet_year,fleet_configuration
  scenario.definition.file <- file.path(city.output.folder, paste0(cityname, "_scenario_definition.csv"))
  # emission factor file
  emission.factors.file <- file.path(city.output.folder, paste0(cityname, "_emission_factors.csv"))
  
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
  
  # !!! There is no guarantee that the emission rasters are made with the 
  # emission factors in the config file when emission rasters from another
  # project are used.

  # check if a gridded network table and emission factors are available
  if (file.exists(gridded.network.file) & file.exists(emission.factors.file)) {

    # read the emission factors
    scenario.efs.df <- read.table(emission.factors.file, sep = ",", header = TRUE)
    scenario.list <- as.vector(unique(scenario.efs.df$scenario_name))
    n.scenarios <- length(scenario.list)
    # read the gridded network
    gridded.network.df <- read.table(gridded.network.file, sep = ";", header = TRUE, quote = "")

    input.list <- list()
    for (i in 1:n.scenarios) {
      scenario.name <- scenario.list[i]
      # check if the emission raster is already calculated
      scenario.output.folder <- file.path(emission.raster.folder, cityname, 
                                         "results", scenario.name)
      scen.emis.raster.file <- file.path(scenario.output.folder,
                                         paste0("emis_", pollutant, ".asc"))
      # if no raster, add scenario to the input list for the cluster
      if (!(file.exists(scen.emis.raster.file))) {
        input.list[[i]] <- list("scenario.name" = scenario.name,
                                "scenario.output.folder" = scenario.output.folder,
                                "pollutant" = pollutant)
      }
    }
    # if there's work to be done, set up a cluster and calculate the rastes
    # for testing create.gridded.emissions(input.list[[2]])
    if (length(input.list) > 0) {
      # Calculate the number of cores
      no_cores <- detectCores()
      # Initiate cluster. Never take more cores than scenarios and one less than available on the machine
      cl <- makeCluster(min(no_cores - 1, n.scenarios))
      # add common variables for all scenarios to the cluster environment
      clusterExport(cl, c("gridded.network.df", "scenario.efs.df", "results.folder",
                          "AADT.field", "emission.raster.folder"))
      # throw the runs on the cluster
      print(paste0("Creating emission rasters for ", cityname))
      parLapply(cl, input.list, create.gridded.emissions)
      # stop the cluster
      stopCluster(cl)
    } else {
      print(paste0("All emission rasters are already calculated for", cityname))
    }

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

  input.list <- list()
  n.scenarios <- length(scenario.list)
  # loop over all scenarios
  for (i in 1:n.scenarios) {
    # path to the emission raster of the scenario
    scen.emis.raster.file <- file.path(emission.raster.folder, cityname, 
                                              "results", scenario.list[i],
                                              paste0("emis_", pollutant, ".asc"))
    scen.conc.raster.file <- file.path(results.folder, scenario.list[i],
                                       paste0(pollutant, "_total_conc.asc"))
    # If the emission raster is available and the result not available, add it 
    # to the list of concentrations to be calculated.
    if (file.exists(scen.conc.raster.file)) {
      print(paste("Concentrations are already calculated:", scen.conc.raster.file))
    } else {
      if (file.exists(scen.emis.raster.file)) {
        # create a scenario folder if it doesn't exitst yet. E.g. when emission
        # rasters are taken from another project.
        scenario.output.folder <- file.path(results.folder, scenario.list[i])
        if (!dir.exists(scenario.output.folder)) { dir.create(scenario.output.folder) }
        # Add all inputs to the imput list
        input.list[[i]] <- list("sc.config.file" = sc.config.file,
                                "city.coords" = city.coords,
                                "emis.raster.file" = scen.emis.raster.file,
                                "pollutant" = if(pollutant=="NOx") {"NO2"} else {pollutant},
                                "output.path" = scenario.output.folder,
                                "raster.background" = raster.background)
      } else {
        print(paste("Emission raster not found at:", scen.emis.raster.file))
      }
    }
  }
  # for testing
  # sherpacity_par(input.list[[3]])
  if (length(input.list) > 0) {
    # Calculate the number of cores
    no_cores <- detectCores() - 1
    # Initiate cluster with as many cores as scenarios if possible, but never more than the
    # total number of cores minus 1
    cl <- makeCluster(min(no_cores, length(input.list)))
    print(paste0("Calculating concentrations for ", length(input.list), " scenarios in ", 
                 cityname, " on ", no_cores, " cores."))
    # throw the runs on the cluster
    parLapply(cl, input.list, sherpacity_par)
    # stop the cluster
    stopCluster(cl)
  } 

#  } # if stuff exists
} # close loop over cities



