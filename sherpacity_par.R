# ----------------------------------------#
# SOURCE RECEPTOR MODEL WITH POINT SOURCE #
# ----------------------------------------#

# # clean up
# rm(list = ls())
# 
# wd <- "D:/SHERPAcity/sherpacitymodel/"
# setwd(wd)


sherpacity_par <- function(scenario.input.list) {
  
  library(raster)
  source("getSourceKernelName.R")
  source("getBackground.R")
  source("Romberg_correlation.R")
  
  # inputs:
  # 1) sc.config.file: path to a configuration file with the paths to the kernel folder and background
  #                    netcdf files
  # 2) city.coord: matrix with 1 row (city name) and two columns, the longitude and latitude of the city
  # 3) emission.raster.file: gridded emissions (kg/h), resolution 20x20m
  # 4) pollutant: 'NO2' or 'PM25': For NO2 the NOx split (NO-NO2) has to be calculated taking into
  #               account the background concentration.
  # 
  # 
  # background.df: data frame with path to raster files with annual average NO2, NO and O3 concentration
  # pollutant: NO2 or PM25
  # output:
  # rasters with NOx and NO2 concentration or PM2.5 concentration.
  
  # unpack the input list
  sc.config.file <- scenario.input.list$sc.config.file
  city.coord <- scenario.input.list$city.coord
  emission.raster.file <- scenario.input.list$emis.raster.file
  pollutant <- scenario.input.list$pollutant
  output.path <- scenario.input.list$output.path

  # Run the configuration file
  # ----------------------------
  source(sc.config.file)

  # Get the source kernel
  # ----------------------
  # get the path to the source kernels foler from the config file
  # source.kernel.folder <- toString(config.df$value[config.df$variable_name == "source.kernel.folder"])
  # get the name of the closest kernel from the position of the city
  source.kernel.file <- file.path(source.kernel.folder, getSourceKernelName(city.coord, "EUR"))
  # read the source kernel raster
  # This is an .asc raster file with concentrations around a point source of 1 kg/h for the city location, 
  # resolution 20x20m
  sk.raster <- raster(source.kernel.file)
  # convert the source kernel in a receptor kernel by point mirroring
  sk.matrix <- as.matrix(sk.raster)
  rk.matrix <- sk.matrix[ncol(sk.matrix):1,][,nrow(sk.matrix):1]
  
  # Read the emission raster file
  # ------------------------------
  emis.raster <- raster(emission.raster.file)
  
  # Get the background concentrations
  # ----------------------------------
  
  # -------- NO2 -----------
  if (pollutant == 'NO2') {
    # Four possibilities
    # NO not available, NOx and NO2 available (the case of Chimere)
    if (is.na(no.varname) & (!(is.na(no2.varname)) | !(is.na(nox.varname)))) {       
      background.info <- matrix(c(background.no2.nc, no2.varname, background.nox.nc, nox.varname),
                                nrow = 2, ncol = 2,
                                dimnames=list(c("nc.file", "varname"), c("NO2", "NOx")))
      # retrieve the background at the city location
      background.matrix <- getBackground(city.coord, background.info)
      # add NO
      no.matrix <- matrix(background.matrix['conc_ugm3', 'NOx'] - background.matrix['conc_ugm3', 'NO2'],
                          nrow = 1, ncol = 1,
                          dimnames = list(c("conc_ugm3"), c("NO")))
      background.matrix <- cbind(background.matrix, no.matrix)
    # NOx not available, NO and NO2 available (the case of Emep)
    } else if (is.na(nox.varname) & (!(is.na(no.varname)) | !(is.na(no2.varname)))) {
      background.info <- matrix(c(background.no.nc, no.varname, background.no2.nc, no2.varname),
                                nrow = 2, ncol = 2,
                                dimnames=list(c("nc.file", "varname"), c("NO", "NO2")))
      # retrieve the background at the city location
      background.matrix <- getBackground(city.coord, background.info)
      # add NO
      nox.matrix <- matrix(background.matrix['conc_ugm3', 'NO'] + background.matrix['conc_ugm3', 'NO2'],
                          nrow = 1, ncol = 1,
                          dimnames = list(c("conc_ugm3"), c("NOx")))
      background.matrix <- cbind(background.matrix, nox.matrix)
    } else if (is.na(no2.varname) & (!(is.na(no.varname)) | !(is.na(nox.varname)))) {
      background.info <- matrix(c(background.no.nc, no.varname, background.nox.nc, nox.varname),
                                nrow = 2, ncol = 2,
                                dimnames=list(c("nc.file", "varname"), c("NO", "NOx")))
      # retrieve the background at the city location
      background.matrix <- getBackground(city.coord, background.info)
      # add NO
      no2.matrix <- matrix(background.matrix['conc_ugm3', 'NOx'] - background.matrix['conc_ugm3', 'NO'],
                           nrow = 1, ncol = 1,
                           dimnames = list(c("conc_ugm3"), c("NO2")))
      background.matrix <- cbind(background.matrix, no2.matrix)
    } else {
      # the background is assumed zero if 2 or 3 variables are NA
      background.matrix <- matrix(rep(0, 3), nrow = 1, ncol = 3,
                           dimnames = list(c("conc_ugm3"), c("NO2", "NO", "NOx")))
    }
    
  # -------- PM2.5 -----------  
  } else if (pollutant == 'PM25') {
    # read netcdf file names and variable names in the netcdf from the config file
    background.pm.nc <- toString(config.df$value[config.df$variable_name == "background.pm25.nc"])
    pm25.varname <- toString(config.df$value[config.df$variable_name == "pm25.varname"])
    # retrieve the background at the city location
    background.info <- matrix(c(background.pm.nc, pm25.varname), nrow=2, ncol=1,
                              dimnames=list(c("nc.file", "varname"), c("PM25")))
    background.matrix <- getBackground(city.coord, background.info)
  
  # -------- PM10 ------------  
  } else if (pollutant == 'PM10') {
    # read netcdf file names and variable names in the netcdf from the config file
    background.pm.nc <- toString(config.df$value[config.df$variable_name == "background.pm25.nc"])
    pm10.varname <- toString(config.df$value[config.df$variable_name == "pm10.varname"])
    # retrieve the background at the city location
    background.info <- matrix(c(background.pm.nc, pm10.varname), nrow=2, ncol=1,
                              dimnames=list(c("nc.file", "varname"), c("PM10")))
    background.matrix <- getBackground(city.coord, background.info)
    
  } else {
    print(paste("unknown pollutant:", pollutant))
  }
  
  write.table(background.matrix, file.path(output.path, 'background.txt'), quote = FALSE)
  
  # Calculate concentrations
  # ------------------------
  
  # focal applies the receptor kernel as a moving average over the emission field
  # conc.raster gives to local contribution to the concentration
  conc.raster <- focal(emis.raster, rk.matrix , pad = TRUE, padValue = 0, na.rm=TRUE)
  conc.raster[is.na(conc.raster)] <- 0
  # plot(conc.raster)
  
  if (pollutant == 'NO2') {
    
    # calculate local NOx as NOx.background - average local NOx over domain + local NOx
    # !!! Also here na.rm=TRUE because NAs are still possible if there are big areas without emissions (sea)
    nox.local.mean <- mean(values(conc.raster), na.rm = TRUE)
    nox.background <- background.matrix["conc_ugm3", "NOx"]
    nox.raster <- conc.raster - nox.local.mean + nox.background
    
    # calculate local NO2 fraction
    local.romberg.coefs <- getRombergCoefs(background.matrix["conc_ugm3", "NO"], background.matrix["conc_ugm3", "NO2"])
    fno2.raster <- raster(nox.raster)
    values(fno2.raster) <- Romberg(values(nox.raster), local.romberg.coefs$a, local.romberg.coefs$b)
  
    # calculate NO2
    no2.raster <- nox.raster * fno2.raster
    
    # write the results to an ascii file
    writeRaster(no2.raster, file.path(output.path, 'NO2_total_conc.asc'), overwrite = TRUE)
    writeRaster(nox.raster, file.path(output.path, 'NOx_total_conc.asc'), overwrite = TRUE)
    writeRaster(conc.raster, file.path(output.path, 'NOx_local_conc.asc'), overwrite = TRUE)
    
  }
  
  if (pollutant %in% c("PM25", "PM10")) {
    # calculate local PM as PM.background - average local PM over domain + local PM
    # In this way double counting is avoided.
    pm.local.mean <- mean(values(conc.raster))
    # write the results to an ascii file
    pm.raster <- conc.raster - pm.local.mean + background.matrix["conc_ugm3", pollutant]
    # write a raster with the total and local concentration
    writeRaster(pm.raster, file.path(output.path, pollutant, "_total_conc.asc"), overwrite = TRUE)
    writeRaster(conc.raster, file.path(output.path, pollutant, "_local_conc.asc"), overwrite = TRUE)
  }
}

