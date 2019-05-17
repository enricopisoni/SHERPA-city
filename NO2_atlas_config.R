# ----------------------
# NO2 Atlas config file
# ----------------------

# The working directory is set in the NO2_atlas_workflow script
# All paths have to be defined with that reference or as absolute paths

# file with list of cities 
city.list.txt <- "city_list.txt"
city.df <- read.table(city.list.txt, header = TRUE, sep = ";")

# path to the OTM shape files scaled to GAINS national totals
otm.path <- "/home/degraba/NO2_atlas/OTM_NUTS3_corrected_20181002/"

# list with boundary boxes of nuts3 areas            
tag <- "20180723"
nuts3.bbox.file <- paste0("/home/degraba/NO2_atlas/OTM_NUTS3_corrected_20181002/OTM_nuts3_boundaryboxes_", tag, ".txt")
nuts3.bbox.df <- read.table(nuts3.bbox.file, sep = ";", header = TRUE)
nuts3.bbox.df <- na.exclude(nuts3.bbox.df)

# output folder in which every city will have a subfolder
cities.output.folder <- "AllCities"

# AADT field to be used for emission calculations: 'trafficvol' or 'capcor'
# trafficvol is the traffic volume of OTM scaled up to match GAINS national totals
# capcor is the road capacity of OTM scaled down to match GAINS national totals
AADT.field <- "trafficvol"

# overwrite existing files for different steps?
rerun.domain <- FALSE
rerun.zoning <- FALSE

# path to the sherpacity configuratoin file
sc.config.file <- "sherpacity_config_emep_linux.R"
pollutant <- "NOx"

