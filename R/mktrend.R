mktrend <-
function(dat){
  require(zyp)
  colnames(dat) <- tl(2010)$m
  #dat <-colreplace(dat, "NaN", NA, cols=1:12)
  ee <- as.data.frame(t(dat))
  ey <- apply(ee, 2, mean, na.rm=TRUE)
  ee <- rbind(ee, ey)
  row.names(ee)[13] <- "year"
  z <- zyp.trend.dataframe(ee, 0)
  mmean <- apply(ee, 1, mean, na.rm=TRUE)
  z$reltrend <- z$trend/mmean
  z
}
