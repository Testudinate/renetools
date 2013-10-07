eb <-
function(xx,yy,ee,ew=NULL, ...){
  if (is.null(ew)){ew <- max(xx, na.rm=TRUE)/200}
  for(i in 1:length(xx)){
    x <- xx[i]
    y <- yy[i]
    e <- ee[i]
    lines(c(x,x),c(y-e,y+e), ...)
    lines(c(x-ew,x+ew),c(y+e,y+e), ...)
    lines(c(x-ew,x+ew),c(y-e,y-e), ...)
  }
}
