# -------------------------------------------------
# Create gridded emissions from the gridded network
# -------------------------------------------------

# rm(list = ls())



# function creating gridded emissions for a scenario
# --------------------------------------------------
create.gridded.emissions <- function(scenario_name) {
  library(rgdal)
  library(raster)
  library(plyr)
  
  # create a scenario folder if it doesn't exitst yet
  scenario.folder <- file.path(results.folder, scenario_name)
  if (!dir.exists(scenario.folder)) { dir.create(scenario.folder) }
  
  # check if the file already exists
  nox.raster.file <- file.path(scenario.folder, "emis_NOx.asc")
  if (!file.exists(nox.raster.file)) {
    
    # select the emission factors for the scenario
    scen.efs.df <- scenario.efs.df[scenario.efs.df$scenario_name == scenario_name,]

    # functional to network conversion
    functional2network <- data.frame(
      functional = c("mainRoad", "firstClass", "secondClass", "thirdClass", "fourthClass"),
      network = c("motorway", "non urban road", "non urban road", "urban road", "urban road"))
    
    # join the link table between functional road classes and road types (network from TREMOVE)
    gridded.network.df <- merge(gridded.network.df, functional2network, c("functional"))
    Ncell <- NROW(gridded.network.df)
    
    # merge the emission factors per road type (network) and zone
    gridded.network.df <- merge(gridded.network.df, scen.efs.df, c("zone_name", "network"))
    
    # Calucate NOx emissions for each road segement in each cell in kg/h as product of:
    #   (trafficvol | capcor) / 24 (AADT/24 = annual average hourly traffic in veh/h)
    #   onroad.ef.nox / 1000 (kg/km)
    #   length_in_cell / 1000 (km)
    # AADT.field is defined in NO2_atlas_config.R
    gridded.network.df$nox.kgh <- gridded.network.df[,AADT.field] / 24 *
      gridded.network.df$onroad.ef.nox.gkm / 1000 *
      gridded.network.df$length_in_cell / 1000
    # gridded.network.df$pm25.kgh <- gridded.network.df[AADT.field,] / 24 *
    #   gridded.network.df$onroad.ef.pmex.gkm / 1000 *
    #   gridded.network.df$length_in_cell / 1000
    
    # aggregate emissions per cell. This takes the most time.
    gridded.emission.df <- ddply(gridded.network.df, c("x_centre", "y_centre"), summarize,
                                nox.cell.kgh = sum(nox.kgh))
                                # pm25.cell.kgh = sum(pm25.kgh))
  
    # aggregate emissions per functional road class
    functional.emission.df <- ddply(gridded.network.df, c("functional"), summarize,
                                 nox.functional.kgh = sum(nox.kgh))
                                 # pm25.functional.kgh = sum(pm25.kgh))
    
    # convert the NOx data.frame > SpatialPointsDataFrame > RasterLayer
    nox.emis.df <- gridded.emission.df[, c("x_centre", "y_centre", "nox.cell.kgh")]
    coordinates(nox.emis.df) <- ~ x_centre + y_centre # SpatialPointsDataFrame 
    gridded(nox.emis.df) <- TRUE # SpatialPointsDataFrame  
    nox.emis.raster <- raster(nox.emis.df) # RasterLayer
    # write the raster to an ascii file
    writeRaster(nox.emis.raster, filename = nox.raster.file, overwrite=TRUE)
  
    # convert the PM2.5 data.frame > SpatialPointsDataFrame > RasterLayer
    # pm25.emis.df <- gridded.emission.df[, c("x_centre", "y_centre", "pm25.cell.kgh")] # data.frame
    # coordinates(pm25.emis.df) <- ~ x_centre + y_centre # SpatialPointsDataFrame
    # gridded(nox.emis.df) <- TRUE
    # nox.emis.raster <- raster(nox.emis.df)
    # writeRaster(nox.emis.raster, filename = paste0(scenario.folder, "/emis_PM25.asc"), overwrite=TRUE)
    return(paste(nox.raster.file, "created."))
  } else {
    return(paste(nox.raster.file, "was already created."))
  }
}

