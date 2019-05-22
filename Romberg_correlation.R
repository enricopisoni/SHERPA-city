# ----------------------------------------
#  Adapted Romberg correlation NOx > fNO2
# ----------------------------------------

# Romberg: fNO2 = a/(NOx+b) + 0.18
# a = 30, b = 35
# for NOx->0 fNO2->1 Seems to be like this in practice, see measurements
# => a/b = 0.82


getRombergCoefs <- function(NO.bg, NO2.bg) {
  c <- 0.18
  d <- 1 - c
  # function gives Romberg correlation coefficients so that 
  # 1) fNO2=1 for NOx.bg=0 and 2) fNO2=fNO2.bg for NOx=NOx.bg
  NOx.bg <- NO.bg + NO2.bg
  fNO2.bg <- NO2.bg / NOx.bg
  a <- NOx.bg / (1/(fNO2.bg - c) - 1/d)
  b <- a / d
  return(data.frame(a = a, b = b))
}

Romberg <- function(NOx, a, b) {
  return(a/(NOx + b) + 0.18)
}

# test
if (1 == 0) {
  NO.bg <- 244.2306
  NO2.bg <- 100.477
  NOx.bg <- NO.bg + NO2.bg
  fNO2.bg <- NO2.bg / NOx.bg
  
  rom.coefs <- getRombergCoefs(NO.bg, NO2.bg)
  
  nox <- 1:400
  plot(nox, Romberg(nox, 30, 35), type = "l", col = "red",
       xlab = "NOx (ug/m3", ylab = "fNO2")
  lines(nox, Romberg(nox, rom.coefs$a, rom.coefs$b), col = "green" )
  points(NOx.bg, fNO2.bg)     
}
