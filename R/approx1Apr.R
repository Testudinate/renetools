approx1Apr <-
function(T10m, what="T10m"){
  res <- NULL           
  jahre <- unique(T10m$year)
  for(i in 1:length(jahre)){
    tdat <- subset(T10m, year == jahre[i])
    res[i] <- approx(tdat$doy, tdat[,what], tl(jahre[i])$d[4])$y
  }
  data.frame(year=jahre, T10m_1Apr=res)
}
