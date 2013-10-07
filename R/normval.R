normval <-
function(x,normmin,normmax){
  xmin <- min(x, na.rm=TRUE)
  xmax <- max(x, na.rm=TRUE)
  (x - xmin) * (normmax - normmin)/(xmax - xmin) + normmin
}
