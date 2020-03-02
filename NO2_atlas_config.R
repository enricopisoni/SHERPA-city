# ----------------------
# NO2 Atlas config file
# ----------------------

# Make a copy of this file and call it 'NO2_atlas_config.R'. Make the necessary changes
# to the paths and variables.

# The working directory is set as the one that contains the 'NO2_atlas_workflow.R' script.
# All paths have to be defined relative to this folder or as absolute paths.

# file with list of cities 
# format:
# cityname;lon;lat;country;lon.min;lon.max;lat.min;lat.max
# Amsterdam;4.898121;52.372268;NL;NA;NA;NA;NA
# Brussels;4.3125;50.84375;BE;4.228685;4.509813;50.773578;50.933293
city.list.txt <- "city_list.txt"
city.df <- read.table(city.list.txt, header = TRUE, sep = ";")

# where are the data? (To be moved to the sherpa city config file)
fleets.file <- "../_fleets/fleets.csv"
fleets.data.file <- "../_fleets/fleets_data.csv"

# fleet configurations
fleet.configuration.folder <- "_fleet_configurations"
fleet.config.overview.file <- "Fleet_configuration_overview.csv"

# scenario definition template
fleet.year <- 2016
scenario.template.file <- "_scenario_definitions/LEZ_Madrid_scenarios.csv"

# path to the OTM shape files scaled to GAINS national totals
otm.path <- "../../OpenTransportMap/OTM_NUTS3_corrected_20181002/OTM_NUTS3_corrected_20181002"
if (!dir.exists(otm.path)) {
  print(paste("The path to the OTM files is not valid:", otm.path))
}
# list with boundary boxes of nuts3 areas            
nuts3.bbox.file <- "../../OpenTransportMap/OTM_nuts3_boundaryboxes_20180723.txt"
if (!file.exists(nuts3.bbox.file)) {
  print(paste("The path to the boundary box file is not valid:", nuts3.bbox.file))
} else {
  nuts3.bbox.df <- read.table(nuts3.bbox.file, sep = ";", header = TRUE)
  nuts3.bbox.df <- na.exclude(nuts3.bbox.df)
}

# cities network folder: this folder contains a sub folder 'cityname'. In this
# sub folder there has to be a shape file 'traffic_roadlinks_<cityname>.shp' and
# eventually a shape file 'traffic_roadlinks_zones_<cityname>'
cities.network.folder <- "AllMadrid"

# cities zones folder
cities.zones.folder <- "AllMadrid"

# emission raster folder. In the subfolder of this folder there should
# be/will be a cityname folder with scenario subfolders
emission.raster.folder <- "AllMadrid"

# output folder in which every city will have a subfolder
cities.output.folder <- "AllMadrid"

# AADT field to be used for emission calculations: 'trafficvol' or 'capcor'
# trafficvol is the traffic volume of OTM scaled up to match GAINS national totals
# capcor is the road capacity of OTM scaled down to match GAINS national totals.
# Field names in a shape file cannot have more than 7 characters.
AADT.field <- "aadtmad"

# overwrite existing files for different steps?
rerun.domain <- FALSE
rerun.zoning <- FALSE

# Which CTM background to use, a scalar (FALSE) or a 20x20m raster interpollated from the CTM data (TRUE)
raster.background <- TRUE

# path to the sherpacity configuratoin file
sc.config.file <- "sherpacity_config_madrid.R"
pollutant <- "NOx"

