# ------------------------------------------------------
# config file for 'sherpacity.R' and 'sherpacity_par.R'
# ------------------------------------------------------

# location of the source kernels
source.kernel.folder <- "/home/degraba/NO2_atlas/_SourceKernels/EUR"

# location of the background netcdfs for NO2, NO, NOx, O3, PM2.5 and PM10
# NO2, NO and NOx have to be specified but one of the three can be NA.
# In that case the third will be calculated from NOx = NO + NO2.
# Units of the tree species has to be ug/m3 NO2eq.
# Projection is WGS84 (latitude, longitude)
background.path <- "/home/degraba/NO2_atlas/_background/Emep2016"
background.no2.nc <- file.path(background.path, "EMEP_run_STANDARD_fullrun.nc")
no2.varname <- "SURF_ug_NO2"        # variable name in the netcdf above 
background.no.nc <- file.path(background.path, "EMEP_run_STANDARD_fullrun.nc")
no.varname <- "SURF_ug_NO"
background.nox.nc <- NA
nox.varname <- NA
# Ozone (not used now, maybe in a correlation for the NO2 fraction)
background.o3.nc <- NA
o3.varname <- NA
# PM
background.pm25.nc <- file.path(background.path, "EMEP_run_STANDARD_fullrun.nc")
pm25.varname <- NA
background.pm10.nc <- file.path(background.path, "EMEP_run_STANDARD_fullrun.nc")
pm10.varname <- NA
# output path for web tool version
output.path <- "Z:/"
