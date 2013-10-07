writetex <-
function(dat, file, digits=2){
  require(xtable)
  towrite <- xtable(dat, digits=digits)
  print(towrite, file=file)
}
