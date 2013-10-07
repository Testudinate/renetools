axisb <-
function(side=1, at, labels, ...){
  ## ticks
  ## length(at) needs to be length(labels) + 1
  axis(side, at=at, labels=FALSE, ...)
  ## labels
  nat <- length(at)
  atlab <- (at[1:(nat-1)] + at[2:nat])/2
  axis(side, at=atlab, labels=labels, tick=FALSE, ...)
}
