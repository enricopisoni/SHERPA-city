# ----------------------
# NO2 Atlas config file
# ----------------------

# Make a copy of this file and call it 'NO2_atlas_config.R'. Make the necessary changes
# to the paths and variables.

# The working directory is set the one that contains the 'NO2_atlas_workflow.R' script.
# All paths have to be defined relative to this folder or as absolute paths.

# file with list of cities 
# format:
# cityname;lon;lat;country;lon.min;lon.max;lat.min;lat.max
# Amsterdam;4.898121;52.372268;NL;NA;NA;NA;NA
# Brussels;4.3125;50.84375;BE;4.228685;4.509813;50.773578;50.933293
city.list.txt <- "city_list.txt"
city.df <- read.table(city.list.txt, header = TRUE, sep = ";")

# path to the OTM shape files scaled to GAINS national totals
otm.path <- "_OpenTransportMap"

# list with boundary boxes of nuts3 areas            
tag <- "20180723"
nuts3.bbox.file <- paste0("_OpenTransportMap/OTM_nuts3_boundaryboxes_", tag, ".txt")
nuts3.bbox.df <- read.table(nuts3.bbox.file, sep = ";", header = TRUE)
nuts3.bbox.df <- na.exclude(nuts3.bbox.df)

# output folder in which every city will have a subfolder
cities.output.folder <- "AllCities or something else or an emtpy string"

# AADT field to be used for emission calculations: 'trafficvol' or 'capcor'
# trafficvol is the traffic volume of OTM scaled up to match GAINS national totals
# capcor is the road capacity of OTM scaled down to match GAINS national totals
AADT.field <- "choose trafficvol or capcor"

# overwrite existing files for different steps?
rerun.domain <- FALSE
rerun.zoning <- FALSE

# path to the sherpacity configuratoin file
sc.config.file <- "sherpacity_config_example.R"
pollutant <- "NOx or PM25 or PM10"

