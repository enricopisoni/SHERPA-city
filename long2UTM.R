# determines UTM code from the longitude (Northern hemisphere only)
long2UTM <- function(long) {
  # example: 8.23 -> "+init=epsg:32632"
  paste0("+init=epsg:326", toString((floor((long + 180)/6) %% 60) + 1))
}
