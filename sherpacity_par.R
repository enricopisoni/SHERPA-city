# ----------------------------------------#
# SOURCE RECEPTOR MODEL WITH POINT SOURCE #
# ----------------------------------------#

# # clean up
# rm(list = ls())
# 
# wd <- "D:/SHERPAcity/sherpacitymodel/"
# setwd(wd)

# !!!!!!!!!!!!!!!!!!!!!
# TO DO:
#   - correct with basecase local contribution for scenarios
#   - update everything for PM
# !!!!!!!!!!!!!!!!!!!!!


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
  myPollutant <- scenario.input.list$pollutant
  output.path <- scenario.input.list$output.path
  raster.background <- scenario.input.list$raster.background

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
  if (myPollutant == 'NO2') {
    # Four possibilities
    # NO not available, NOx and NO2 available (the case of Chimere)
    if (is.na(no.varname) & (!(is.na(no2.varname)) | !(is.na(nox.varname)))) {       
      background.info <- matrix(c(background.no2.nc, no2.varname, background.nox.nc, nox.varname),
                                nrow = 2, ncol = 2,
                                dimnames=list(c("nc.file", "varname"), c("NO2", "NOx")))
      # retrieve the background at the city location
      background.matrix <- getBackground(city.coord, background.info, background.no2.nc, no2.varname)
      # add NO
      background.list[["NO"]] <- background.list[["NOx"]] - background.list[["NO2"]]
      background.list[["NO_centre"]] <- background.list[["NOx_centre"]] - background.list[["NO2"]]
      
    # NOx not available, NO and NO2 available (the case of Emep)
    } else if (is.na(nox.varname) & (!(is.na(no.varname)) | !(is.na(no2.varname)))) {
      background.info <- matrix(c(background.no.nc, no.varname, background.no2.nc, no2.varname),
                                nrow = 2, ncol = 2,
                                dimnames=list(c("nc.file", "varname"), c("NO", "NO2")))
      # retrieve the background at the city location
      background.list <- getBackground(city.coord, background.info, emis.raster, raster.background)
      # add NOx
      background.list[["NOx"]] <- background.list[["NO"]] + background.list[["NO2"]]
      background.list[["NOx_centre"]] <- background.list[["NO_centre"]] + background.list[["NO2_centre"]]
      
    } else if (is.na(no2.varname) & (!(is.na(no.varname)) | !(is.na(nox.varname)))) {
      background.info <- matrix(c(background.no.nc, no.varname, background.nox.nc, nox.varname),
                                nrow = 2, ncol = 2,
                                dimnames=list(c("nc.file", "varname"), c("NO", "NOx")))
      # retrieve the background at the city location
      background.matrix <- getBackground(city.coord, background.info, emis.raster, raster.background)
      # add NO2
      background.list[["NO2"]] <- background.list[["NOx"]] - background.list[["NO"]]
      background.list[["NO2_centre"]] <- background.list[["NOx_centre"]] - background.list[["NO_centre"]]

    } else {
      # the background is assumed zero if 2 or 3 variables are NA
      background.list <- list("NO2" = 0, "NO" = 0, "NOx" = 0)
    }
    
  # -------- PM2.5 -----------  
  } else if (myPollutant == 'PM25') {
    # retrieve the background at the city location
    background.info <- matrix(c(background.pm25.nc, pm25.varname), nrow=2, ncol=1,
                              dimnames=list(c("nc.file", "varname"), c("PM25")))
    background.list <- getBackground(city.coord, background.info, emis.raster, raster.background)
  
  # -------- PM10 ------------  
  } else if (myPollutant == 'PM10') {
    # retrieve the background at the city location
    background.info <- matrix(c(background.pm10.nc, pm10.varname), nrow=2, ncol=1,
                              dimnames=list(c("nc.file", "varname"), c("PM10")))
    background.list <- getBackground(city.coord, background.info, emis.raster, raster.background)
    
  } else {
    print(paste("unknown pollutant:", myPollutant))
  }
  
  # if the low resolution part from the CTM are numbers write a file, if it is a high
  # resolution raster plot the raster.
  if (raster.background == FALSE) {
    write.table(background.matrix, file.path(output.path, 'background.txt'), quote = FALSE)
  } else if (raster.background == TRUE) {
    for (varname in names(background.list)) {
      if (class(background.list[[varname]]) == "RasterLayer") {
        png(file.path(output.path, paste0('CTM_background_', varname, '.png')))
        plot(background.list[[varname]], main = paste(varname, "from the CTM"))
        dev.off()
      }
    }
  }
  
  # Calculate concentrations
  # ------------------------
  
  # focal applies the receptor kernel as a moving average over the emission field
  # conc.raster is the local contribution to the concentration
  conc.raster <- focal(emis.raster, rk.matrix, pad = TRUE, padValue = 0, na.rm=TRUE)
  conc.raster[is.na(conc.raster)] <- 0
  # plot(conc.raster)
  
  if (myPollutant == 'NO2') {
    
    # Combine background and local concentration.
    # Two possibilities: CTM background with or without traffic emissions
    # !!! Also here na.rm=TRUE because NAs are still possible if there are big areas without emissions (sea)
    # get the background from the CTM: an single number or raster
    nox.background <- background.list$NOx
    if (background.includes.traffic == TRUE) {
      # if the background includes traffic a double counting correction is needed substracting
      # the local mean concentration.
      # Calculate the local mean: 2 possibilities; one number are a smoothed background
      if (raster.background == FALSE) {
        nox.local.mean <- mean(values(conc.raster), na.rm = TRUE)
      } else if (raster.background == TRUE) {
        # smooth the local contribution at about 7x7 km
        ncol <- round(background.list$dx / 20)
        if (ncol %% 2 == 0) {ncol <- ncol + 1}
        nrow <- round(background.list$dy / 20)
        if (nrow %% 2 == 0) {nrow <- nrow + 1}
        nox.local.mean <- focal(conc.raster, 
                                w = matrix(1/ncol/nrow, nrow = nrow, ncol = ncol), 
                                pad = TRUE, padValue = NA, na.rm=TRUE)
        png(file.path(output.path, paste0("local_smoothed_NOx.png")))
        plot(nox.local.mean, main = "Smoothed local NOx")
        dev.off()
      }
      # Apply the double counting correction
      # total NOx = Background corrected reduced with smoothed local (number or raster) plus local NOx
      nox.raster <- nox.background - nox.local.mean + conc.raster
    } else if (background.includes.traffic == FALSE) {
      # total NOx = One background value or smoothed background raster plus local NOx
      nox.raster <- nox.background + conc.raster
    }
    
    # calculate local NO2 fraction
    local.romberg.coefs <- getRombergCoefs(background.list[["NO_centre"]], background.list[["NO2_centre"]])
    fno2.raster <- raster(nox.raster)
    values(fno2.raster) <- Romberg(values(nox.raster), local.romberg.coefs$a, local.romberg.coefs$b)
  
    # calculate NO2
    no2.raster <- nox.raster * fno2.raster
    
    # write the results to an ascii file
    writeRaster(no2.raster, file.path(output.path, 'NO2_total_conc.asc'), overwrite = TRUE)
    writeRaster(nox.raster, file.path(output.path, 'NOx_total_conc.asc'), overwrite = TRUE)
    writeRaster(conc.raster, file.path(output.path, 'NOx_local_conc.asc'), overwrite = TRUE)
  }
  
  if (myPollutant %in% c("PM25", "PM10")) {
    # calculate local PM as PM.background - average local PM over domain + local PM
    # In this way double counting is avoided.
    pm.local.mean <- mean(values(conc.raster))
    # write the results to an ascii file
    if (background.includes.traffic == TRUE) {
      pm.raster <- conc.raster - pm.local.mean + background.matrix["conc_ugm3", myPollutant]
    } else if (background.includes.traffic == FALSE) {
      pm.raster <- conc.raster + background.matrix["conc_ugm3", myPollutant]
    }
    # write a raster with the total and local concentration
    writeRaster(pm.raster, file.path(output.path, myPollutant, "_total_conc.asc"), overwrite = TRUE)
    writeRaster(conc.raster, file.path(output.path, myPollutant, "_local_conc.asc"), overwrite = TRUE)
  }
}

