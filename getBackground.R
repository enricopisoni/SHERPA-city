# ------------------------------------------------------------
# function to retrive background concentrations from a netcdf
# ------------------------------------------------------------


library(raster)

getBackground <- function(city.coord, background.info) {
  # Function to retrieve background NO2, NO and O3 concentrations for a given point (lon, lat) from a netcdf file
  # input
  # city.coord: 1 row-2 col matrix with longitude and latitude of the city centre
  # background.info: matrix with 3 columns (NO2, NO, O3) and 2 rows (path to netcf with background and varname)
  
  # output:
  # background.matrix: 3 colunms ((NO2, NO, O3)), 1 row with concentrations in ug/m3 yearly average
  pollutant.list <- dimnames(background.info)[[2]]
  n.pollutants <- length(pollutant.list)
  background.matrix <- matrix(rep(NA, n.pollutants), 
                              ncol=n.pollutants, nrow=1, 
                              dimnames=list(c("conc_ugm3"), pollutant.list))
  
  # pollutant <- "NOx" # for testing
  for (pollutant in pollutant.list) {
    bg.nc <- background.info["nc.file", pollutant]
    pollutant.raster <- raster(bg.nc, varname=background.info["varname", pollutant])
    pol.bg.city <- extract(pollutant.raster, city.coord, method="bilinear")
    background.matrix["conc_ugm3", pollutant] <- pol.bg.city
  }
  return(background.matrix)
}

# for testing

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



