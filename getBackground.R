# ------------------------------------------------------------
# function to retrive background concentrations from a netcdf
# ------------------------------------------------------------

# background = low resolution concentration of the CTM
# This low resolution concentration contains a traffic part an other contributions
# We want to replace the traffic part at low resolution with a traffic part at high
# resolution. How?
# 1) Get something from the CTM:
#    - interpollated value at the centre of the domain
#    - interpollated value at every grid point at high resolution
#    - the previous one averaged over the domain
# 2) Remove the traffic from the low resolution concentration:
#    - single CTM value minus average local traffic concentration (fast)
#    - CTM values at eacht high res grid point minus smoothed local concentration.
#      Smoothed because that is what the CTM does with all concentrations
# 3) Add local concentration to:
#    - single value difference
#    - raster difference

library(raster)

getBackground <- function(city.coord, background.info, emis.raster, raster.background) {
  
  # Function to retrieve background NO2, NO and O3 concentrations for a given point (lon, lat) from a netcdf file
  # input
  # city.coord: 1 row-2 col matrix with longitude and latitude of the city centre
  # background.info: matrix with 3 columns (NO2, NO, O3) and 2 rows (path to netcf with background and varname)
  # Two possibilities:
  #   1) raster.background=TRUE
  #      The result is a raster at 20x20 m resultion interpollated from the background netcdf
  #   2) raster.background=FALSE
  #      The result is a scalar interpollated from the background netcdf (centre)
  # output:
  # background.list: n.pollutant elements, scalar or raster depending on 'raster.background'
  # with concentrations in ug/m3 yearly average.
  
  pollutant.list <- dimnames(background.info)[[2]]
  n.pollutants <- length(pollutant.list)
  background.list <- list()
  
  if (raster.background == FALSE) {
    # return one value per pollutant, interpollated from the background netcdf
    for (pollutant in pollutant.list) {
      bg.nc <- background.info["nc.file", pollutant]
      pollutant.raster <- raster(bg.nc, varname=background.info["varname", pollutant])
      pol.bg.city <- extract(pollutant.raster, city.coord, method="bilinear")
      background.list[[pollutant]] <- pol.bg.city
    }
  } else if (raster.background == TRUE) {
    # return a full raster per pollutant, interpollated from the background netcdf
    for (pollutant in pollutant.list) {
      bg.nc <- background.info["nc.file", pollutant]
      pollutant.raster <- raster(bg.nc, varname=background.info["varname", pollutant])
      
      city.epsg <- long2UTM(city.coord[1,"lon"])
      emis.raster[is.na(emis.raster)] <- 0 # this makes sure that also points with NA are in the coords list
      raster.utm.coords <- rasterToPoints(emis.raster)
      raster.utm.coords <- raster.utm.coords[,c('x', 'y')]
      raster.utm.spdf <- SpatialPointsDataFrame(coords = raster.utm.coords,
                                                data = as.data.frame(raster.utm.coords),
                                                proj4string = CRS(city.epsg))
      raster.wgs84.spdf <- spTransform(raster.utm.spdf, CRS("+init=epsg:4326"))
      # data frame with coordinates of raster points and interpollated CTM concentrations
      ctm.conc.df <- data.frame(raster.utm.coords,
                                conc = extract(pollutant.raster, raster.wgs84.spdf@coords, 
                                               method="bilinear"))
      # convert the ctm concentration data.frame > SpatialPointsDataFrame > RasterLayer
      coordinates(ctm.conc.df) <- ~ x + y # SpatialPointsDataFrame 
      gridded(ctm.conc.df) <- TRUE # SpatialPointsDataFrame  
      ctm.conc.raster <- raster(ctm.conc.df) # RasterLayer
      # add the CTM concentration raster to the output list
      background.list[[pollutant]] <- ctm.conc.raster
    }
  }
  return(background.list)
}

# for testing
# emis.raster <- raster("D:/SHERPAcity/NO2_atlas/smooth_background/AllCities/Ljubljana/results/basecase/emis_NOx.asc")

if (1 == 0) {
  # test example with LOTOS background
  city.lon <- 9.1894
  city.lat <- 45.4642
  background.path <- "D:/ROADMOD/LOTOS EUROS simulations/lotos_euros_results/Roadmod Scenarios/_summary_netcdfs/"
  background.nc <- paste0(background.path, "LE_EUR_tremove_baseline_2010_conc-sfc_2009.nc")
  city.coord <- matrix(c(city.lon, city.lat), ncol=2, dimnames = list(c('city'), c("lon", "lat")))
  background.info <- matrix(c(background.nc, "no2_year_avg", background.nc, "no_year_avg", background.nc, "o3_year_avg"), 
                            nrow=2, ncol=3,
                            dimnames=list(c("nc.file", "varname"), c("NO2", "NOx", "O3")))
  background.conc <- getBackground(city.coord, background.info)
  
  
  # test example with CHIMERE background for SHERPA
  background.path <- "O:/Integrated_assessment/SHERPA/20170322_v18_SrrResults_PotencyBased/2_base_concentrations/"
  background.no2.nc <- paste0(background.path, "BC_conc_NO2_NO2eq_Y_mgm3.nc")
  background.nox.nc <- background.no2.nc
  background.o3.nc <- paste0("Z:/pisonen/CHIMERE_DATA/CHIMERE-CTM4IAM-YearMonth-EmissAqi/2010Cle_TSAP_Dec_2013_JRC01_07b_2009/", "outl.2010Cle_TSAP_Dec_2013_JRC01_07b_2009_JRC07b_O3_Y.nc")
  city.coord <- matrix(c(city.lon, city.lat), ncol=2, dimnames = list(c('city'), c("lon", "lat")))
  background.info <- matrix(c(background.no2.nc, "NO2", background.nox.nc, "conc", background.o3.nc, "O3"), 
                            nrow=2, ncol=3,
                            dimnames=list(c("nc.file", "varname"), c("NO2", "NO", "O3")))
  background.conc <- getBackground(city.coord, background.info)
  # NO is actually NOx
  background.conc['conc_ugm3', 'NO'] <- background.conc['conc_ugm3', 'NO'] - background.conc['conc_ugm3', 'NO2']
  
  # test example with CHIMERE background
  # Chimere backgrounds
  # Ozone: Z:/pisonen/CHIMERE_DATA/CHIMERE-CTM4IAM-YearMonth-EmissAqi/2010Cle_TSAP_Dec_2013_JRC01_07b_2009/outl.2010Cle_TSAP_Dec_2013_JRC01_07b_2009_JRC07b_O3_Y.nc
  background.path <- "Z:/pisonen/CHIMERE_DATA/CHIMERE-CTM4IAM-YearMonth-EmissAqi/2010Cle_TSAP_Dec_2013_JRC01_07b_2009/"
  background.no2.nc <- paste0(background.path, "BC_conc_NO2_NO2eq_Y_mgm3.nc")
  background.o3.nc <- paste0(background.path, "outl.2010Cle_TSAP_Dec_2013_JRC01_07b_2009_JRC07b_O3_Y.nc")
  city.coord <- matrix(c(city.lon, city.lat), ncol=2, dimnames = list(c('city'), c("lon", "lat")))
  background.info <- matrix(c(background.no2.nc, "NO2", background.no.nc, "conc", background.o3.nc, "O3"), 
                            nrow=2, ncol=3,
                            dimnames=list(c("nc.file", "varname"), c("NO2", "NO", "O3")))
  background.conc <- getBackground(city.coord, background.info)  
}



