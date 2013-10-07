getzmix <-
function(z, temp, z10=FALSE, thyp=10, approx=FALSE){
  if(approx){
    depths <- seq(range(z)[1], range(z)[2], 0.1)
    temps  <- approx(z, temp, depths)$y
  } else {
    depths <- z
    temps  <- temp
  }
  t2m    <- approx(z, temp, 2)$y
  tempzmix  <- tzmix(t2m)
  epilimnion <- which(temps >= tempzmix)
  zmix <- depths[epilimnion[length(epilimnion)]]
  if(length(zmix)<1){zmix <- max(depths)}
  if(z10){
    metalimnion <- which(temps >= thyp)
    z10 <- depths[metalimnion[length(metalimnion)]]
    zmix[2] <- z10
  }
  zmix
}
