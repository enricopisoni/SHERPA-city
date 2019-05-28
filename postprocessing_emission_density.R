# --------------------------------
# Postprocessing: emission density 
# --------------------------------

# Clean up
rm(list=ls())
# set the directory of this script as working directory
wd <- dirname(sys.frame(1)$ofile)
setwd(wd)

library(raster)
library(plyr)
library(ggplot2)
source("NO2_atlas_config.R")
source("long2UTM.R")

# emission inventories used by CTMs (units: "Mg/km2")
emep.emissions.nc <- "Z:/pisonen/SHERPA/pm25_slidingWindows_emepChimere/chimere/BC_emi_PM25_Y.nc"
emep.nox.snap7.r <- raster(emep.emissions.nc, varname="NOx", band=7) # "Mg/km2";
# NOx of all SNAP sectors
emep.nox.allsnap.r <- emep.nox.snap7.r*0
for (snap in 1:10) {
  emep.nox.snap.r <- raster(emep.emissions.nc, varname="NOx", band=snap) # "Mg/km2";
  emep.nox.allsnap.r <- emep.nox.allsnap.r + emep.nox.snap.r
}
chimere.emissions.nc <- "Z:/pisonen/SHERPA/pm25_slidingWindows_emepChimere/emep/full domain/BC_emi_PM25_Y_SNAP.nc"
chimere.nox.snap7.r <- raster(chimere.emissions.nc, varname="NOx", band=7) # "Mg/km2"
for (snap in 1:10) {
  emep.nox.snap.r <- raster(emep.emissions.nc, varname="NOx", band=snap) # "Mg/km2";
  emep.nox.allsnap.r <- emep.nox.allsnap.r + emep.nox.snap.r
}

chimere.raster.points <- rasterToPoints(chimere.nox.snap7.r)
chimere.raster.points[is.na(chimere.raster.points[,"NOx"])] <- 0
chimere.raster.spdf <- SpatialPointsDataFrame(coords = chimere.raster.points[,c("x", "y")],
                                              data = data.frame(NOx = chimere.raster.points[,"NOx"]),
                                              proj4string = CRS("+init=epsg:4326"))
emep.NOx.at.chimere <- extract(emep.nox.snap7.r, chimere.raster.points[,c("x", "y")], method="bilinear")
emep.NOx.at.chimere[is.na(emep.NOx.at.chimere)] <- 0
png(file.path(wd, "_postprocessing", "NOx_Emissions_comparison.png"), width = 2*480, height = 480)
par(mfrow=c(1,2))
plot(chimere.raster.points[,"NOx"], emep.NOx.at.chimere,
     xlab = "Chimere NOx emissions (Mg/km2)", ylab = "Emep NOx emissions (Mg/km)")
abline(a=0, b=1, col="red")

plot(chimere.raster.points[,"NOx"], emep.NOx.at.chimere,
     xlab = "Chimere NOx emissions (Mg/km2)", ylab = "Emep NOx emissions (Mg/km)",
     xlim = c(0.01,100), ylim = c(0.01,100), log="xy", pch=19, cex=0.01)
abline(a=0, b=1, col="red")
dev.off()
cor.emis <- cor(chimere.raster.points[,"NOx"], emep.NOx.at.chimere, method = "pearson")


# cityname <- "Wroclaw"
nox.emission.overview.df <- data.frame()
for (cityname in as.vector(city.df$cityname)) {
  print(cityname)
  # city coordinates and CRS
  city.coords <- matrix(c(city.df$lon[city.df$cityname == cityname],
                          city.df$lat[city.df$cityname == cityname]), ncol = 2,
                        dimnames = list(c(cityname), c("lon", "lat")))
  city.epsg <- CRS(long2UTM(city.coords[1,"lon"]))
  
  # Are there results
  city.output.folder <- sub("^/", "", file.path(cities.output.folder, cityname))
  city.results.folder <- file.path(city.output.folder, "results")
  city.basecase.folder <- file.path(city.results.folder, "basecase")
  emis.NOx.nc <- file.path(city.basecase.folder, "emis_NOx.asc")
  if (file.exists(emis.NOx.nc)) {
    emis.nox.r <- raster(emis.NOx.nc)
    emis.nox.r[is.na(emis.nox.r)] <- 0
    nox.total.sherpacity <- sum(values(emis.nox.r))*24*365/1000 # ton/year
    
    emis.nox.points <- rasterToPoints(emis.nox.r)
    emis.nox.utm.spdf <- SpatialPointsDataFrame(coords = emis.nox.points[,c("x", "y")],
                                                data = data.frame(NOx.sherpacity = emis.nox.points[,"emis_NOx"]),
                                                proj4string = city.epsg)
    emis.nox.wgs84.spdf <- spTransform(emis.nox.utm.spdf, CRS("+init=epsg:4326"))
    
    # get chimere NOx emission density at the sherpa city grid points
    emis.nox.chimere <- extract(chimere.nox.snap7.r, emis.nox.wgs84.spdf@coords, method="simple")
    nox.total.chimere <- sum(emis.nox.chimere)*0.02^2
    
    
    # get emep NOx emission density at the sherpa city grid points
    emis.nox.emep <- extract(emep.nox.snap7.r, emis.nox.wgs84.spdf@coords, method="simple")
    nox.total.emep <- sum(emis.nox.emep)*0.02^2
    
    nox.emission.overview.df <- rbind(nox.emission.overview.df,
                                      data.frame(cityname = cityname,
                                                 nox.total.sherpacity.tonperyear = nox.total.sherpacity,
                                                 nox.total.chimere.tonperyear = nox.total.chimere,
                                                 nox.total.emep.tonperyear = nox.total.emep))
  }
}

output.dir <- file.path(wd, "_postprocessing", "NOx_emission_comparison")
if (!(dir.exists(output.dir))) {create.dir(file.path(wd, "_postprocessing", "NOx_emission_comparison"))}
write.table(nox.emission.overview.df, file = file.path(output.dir, "NOx_Emissions_comparison.csv"),
            sep = ",", row.names = F)

# chimere
plot(nox.emission.overview.df$nox.total.sherpacity.tonperyear,
     nox.emission.overview.df$nox.total.chimere.tonperyear)
abline(a=0, b=1)
text(x = nox.emission.overview.df$nox.total.sherpacity.tonperyear,
     y = nox.emission.overview.df$nox.total.chimere.tonperyear,
     labels = nox.emission.overview.df$cityname)
cor(nox.emission.overview.df$nox.total.sherpacity.tonperyear,
    nox.emission.overview.df$nox.total.chimere.tonperyear)
lm(data = nox.emission.overview.df, formula = nox.total.chimere.tonperyear ~ 0 + nox.total.sherpacity.tonperyear)

# emep
plot(nox.emission.overview.df$nox.total.sherpacity.tonperyear,
     nox.emission.overview.df$nox.total.emep.tonperyear)
abline(a=0, b=1)
text(x = nox.emission.overview.df$nox.total.sherpacity.tonperyear,
     y = nox.emission.overview.df$nox.total.emep.tonperyear,
     labels = nox.emission.overview.df$cityname)
cor(nox.emission.overview.df$nox.total.sherpacity.tonperyear,
     nox.emission.overview.df$nox.total.emep.tonperyear)
lm(data = nox.emission.overview.df, formula = nox.total.emep.tonperyear ~ 0 + nox.total.sherpacity.tonperyear)

plot(nox.emission.overview.df$nox.total.chimere.tonperyear,
     nox.emission.overview.df$nox.total.emep.tonperyear)
abline(a=0, b=1)
text(x = nox.emission.overview.df$nox.total.chimere.tonperyear,
     y = nox.emission.overview.df$nox.total.emep.tonperyear,
     labels = nox.emission.overview.df$cityname)
cor(nox.emission.overview.df$nox.total.chimere.tonperyear,
    nox.emission.overview.df$nox.total.emep.tonperyear)
lm(data = nox.emission.overview.df, formula = nox.total.chimere.tonperyear ~ 0 + nox.total.emep.tonperyear)
