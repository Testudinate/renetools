createfield <-
function(dat, approxmethod="constant", approxside=0){#x=doy, y=dep, z=bm
  require(reshape)
  mdat <- melt(dat, id=c("x", "y"), measure="z")
  cdat <- cast(mdat, y ~ x, fun.aggregate=mean)
  ccdat <- as.matrix(cdat)
  for(i in 1:ncol(ccdat)){
    x <- as.numeric(row.names(ccdat))
    y <- ccdat[,i]
    xnew <- x
    nna <- length(which(!is.na(ccdat[,i])))
    if(nna > 1){
      ynew <- approx(x,y,xnew,method=approxmethod, f=approxside)$y
      ccdat[,i] <- ynew
    }
  }
  ccdat
}
