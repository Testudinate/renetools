lastdoy <-
function(year){
  date <- paste(year,"-12-31", sep="")
  datex(as.POSIXct(date), "%j")
}
