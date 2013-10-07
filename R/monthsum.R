monthsum <-
function(dat, y="tair_calc"){
  require(reshape)
  datag   <- aggregate(list(var=dat[,y]), list(month=dat$month, year=dat$year), sum)
  datmelt <- melt(datag, c("year", "month"), c("var"))
  datcast <- cast(datmelt, year ~ month ~ variable)
  datcast <- as.data.frame(datcast)
  colnames(datcast) <- tl(2000)$m
  datcast
}
