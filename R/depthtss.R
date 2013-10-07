depthtss <-
function(V){
  xnew <- seq(0,46,0.001)
  voltssrev  <- approxfun(voltss(xnew),xnew)
  z        <- voltssrev(V)
  z
}
