# -------------------------------------------
# Comparison of emission intensities between:
# - EMEP inventory
# - CHIMERE inventory
# - OTM trafficvol
# - OTM capacity
# -------------------------------------------

library(raster)
source("long2UTM.R")
source("NO2_atlas_config.R")

wd <- "D:/SHERPAcity/NO2_atlas/run20190731_SNAP7zero"
setwd(wd)

# EMEP emissions, get total NOx and traffic NOx
Emis.GNFR.nc <- "Z:/meijaal/GNFR_Emissions/GNFRemis_EMEP01_2015.nc"
for (i.sector in 1:13) {
  sector.varname <- paste0("nox_sec", sprintf("%02d", i.sector))
  print(paste("Reading", sector.varname, "from", Emis.GNFR.nc))
  NOx.Emis.sector.r <- raster(Emis.GNFR.nc, varname = sector.varname)
  if (i.sector == 1) {
    NOx.Emis.GNFR.r <- NOx.Emis.sector.r
  } else {
    NOx.Emis.GNFR.r <- NOx.Emis.GNFR.r + NOx.Emis.sector.r
  }
  if (i.sector == 6) {
    NOx.Emis.traffic.r <- NOx.Emis.sector.r
  }
}
plot(NOx.Emis.traffic.r / NOx.Emis.GNFR.r)  

# NOx, NO and NO2 concentrations, with and without traffic emissions
# ------------------------------------------------------------------
background.path <- "D:/SHERPAcity/NO2_atlas/_background/EmepSNAP7zero"
# background NOx without traffic
NO2.SNAP7zero.r <- raster(file.path(background.path, "EMEP_run_SNAP7_ZERO_50m_fullrun.nc"),
                          varname = "SURF_ug_NO2")
NO.SNAP7zero.r <- raster(file.path(background.path, "EMEP_run_SNAP7_ZERO_50m_fullrun.nc"),
                         varname = "SURF_ug_NO")
NOx.SNAP7zero.r <- NO2.SNAP7zero.r + NO.SNAP7zero.r
# background NOx with traffic
NO2.basecase.r <- raster(file.path(background.path, "EMEP_run_STANDARDnew_50m_fullrun.nc"),
                         varname = "SURF_ug_NO2")
NO.basecase.r <- raster(file.path(background.path, "EMEP_run_STANDARDnew_50m_fullrun.nc"),
                        varname = "SURF_ug_NO")
NOx.basecase.r <- NO2.basecase.r + NO.basecase.r

# Potency delta_NOx_conc / delta_NOx_emis
# ---------------------------------------
Emep.conc.extent <- extent(NO2.SNAP7zero.r)
NOx.Emis.GNFR.r <- crop(NOx.Emis.GNFR.r, Emep.conc.extent)
NOx.Emis.traffic.r <- crop(NOx.Emis.traffic.r, Emep.conc.extent)
NOx.Emis.dens.traffic.r <- NOx.Emis.traffic.r / area(NOx.Emis.traffic.r) # ton/year/km2

rel.potency.NOx <- (NOx.SNAP7zero.r / NOx.basecase.r - 1) / (NOx.Emis.traffic.r / NOx.Emis.GNFR.r - 1)
plot(log(rel.potency.NOx))


hist(log(values(rel.potency.NOx)))

# SHERPAcity emissions
# --------------------
emission.comparison.df <- data.frame()
# cityname <- "Amsterdam"
for (cityname in as.vector(city.df$cityname)) {
  # city coordinates 
  city.coords <- matrix(c(city.df$lon[city.df$cityname == cityname],
                          city.df$lat[city.df$cityname == cityname]), ncol = 2,
                        dimnames = list(c(cityname), c("lon", "lat")))
  city.utm <- long2UTM(city.coords[1])
  
  # read the SHERPAcity basecase NOx emissions from traffic.
  NOx.emis.basecase.asc <- file.path(emission.raster.folder, cityname, "results", "basecase", "emis_NOx.asc") 
  NOx.emis.basecase.r <- raster(NOx.emis.basecase.asc) # kg/h
  NOx.emis.basecase.ton <- sum(values(NOx.emis.basecase.r), na.rm = T) * 24 * 365 / 1000 # ton
  sherpacity.points <- rasterToPoints(NOx.emis.basecase.r)
  sherpacity.points.utm.spdf <- SpatialPointsDataFrame(coords = sherpacity.points[,c("x", "y")],
                                                   data = data.frame(NOx = sherpacity.points[,"emis_NOx"]),
                                                   proj4string = CRS(city.utm))
  sherpacity.points.wgs84.spdf <- spTransform(sherpacity.points.utm.spdf, CRS("+init=epsg:4326"))
  NOx.Emis.dens.traffic.in.city <- extract(NOx.Emis.dens.traffic.r, 
                                           sherpacity.points.wgs84.spdf@coords,
                                           method = "bilinear")
  NOx.emis.GNFR.ton <- sum(NOx.Emis.dens.traffic.in.city) * 0.020^2 
  
  emission.comparison.df <- rbind(emission.comparison.df,
                                  data.frame(cityname = cityname,
                                             SC.NOx.ton = NOx.emis.basecase.ton,
                                             GNFR.NOx.ton = NOx.emis.GNFR.ton))
  write.table(emission.comparison.df, file.path("_emissions_comparison", "NOx_emissions_comparison.csv"), 
              row.names = F, quote = F, sep = ",")

}
