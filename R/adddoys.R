adddoys <-
function(dat, col=1){
  dat$doy   <- datex(dat[,col], "%j")
  dat$month <- datex(dat[,col], "%m")
  dat$year  <- datex(dat[,col], "%Y")
  dat
}
