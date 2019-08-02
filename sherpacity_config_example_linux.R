# ------------------------------------------------------
# config file for 'sherpacity.R' and 'sherpacity_par.R'
# ------------------------------------------------------

# location of the source kernels
source.kernel.folder <- "../_SourceKernels/EUR"

# location of the background netcdfs for NO2, NO, NOx, O3, PM2.5 and PM10
# NO2, NO and NOx have to be specified but one of the three can be NA.
# In that case the third will be calculated from NOx = NO + NO2.
# Units of the tree species has to be ug/m3 NO2eq.
# Projection is WGS84 (latitude, longitude)
background.path <- "../_background/EmepSNAP7zero"

# background includes traffic? If TRUE a background correction is done,
# otherwise not
background.includes.traffic <- FALSE

emep.output.file <- "EMEP_run_SNAP7_ZERO_50m_fullrun.nc"
background.no2.nc <- file.path(background.path, emep.output.file)
no2.varname <- "SURF_ug_NO2"        # variable name in the netcdf above 
background.no.nc <- file.path(background.path, emep.output.file)
no.varname <- "SURF_ug_NO"
background.nox.nc <- NA
nox.varname <- NA
# Ozone (not used now, maybe in a correlation for the NO2 fraction)
background.o3.nc <- NA
o3.varname <- NA
# PM
background.pm25.nc <- file.path(background.path, emep.output.file)
pm25.varname <- NA
background.pm10.nc <- file.path(background.path, emep.output.file)
pm10.varname <- NA
# output path for web tool version
output.path <- "Z:/"
