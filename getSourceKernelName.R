# -----------------------#
# Get source kernel name #
# -----------------------#

# This function gives the name of the closest source kernel for a
# given longitude and latitude.

# The naming of the source kernel files is as folows:
# "N" + <lat> + "E" or "W" + <abs(lon)> + ".asc"
# lon and lat are in a format xx.xxx with leading and trailing zeros
# A kernel is availabe every 0.5 degrees longitude and every 0.25 degrees latitude.

# for the Benelux, Po Valley and South Poland higher resolution kernels are available.

getSourceKernelName <- function(city.coord, domain) {
  lon <- city.coord[,'lon']
  lat <- city.coord[,'lat']
  if (lon > 0) {
    east.west <- "E"
  } else {
    east.west <- "W"
  }
  if (domain == "EUR") {
    dlon <- 0.5
    dlat <- 0.25
  }
  sk.lon <- sprintf("%06.3f", abs(floor(lon / dlon) * dlon + dlon/2))
  sk.lat <- sprintf("%06.3f", floor(lat / dlat) * dlat + dlat/2)
  SKName <- paste0("SK_N", sk.lat, east.west, sk.lon, ".asc")
  return(SKName)
}

# test
if (1 == 0){
  # city location
  city.lon <- 9.22
  city.lat <- 45.46
  city.name <- "Milan"
  city.coord <- matrix(c(city.lon, city.lat), ncol=2, dimnames = list(c(city.name), c("lon", "lat")))
  domain <- "EUR"
  getSourceKernelName(city.coord, "EUR")
}
