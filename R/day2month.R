day2month <-
function(timedays, year){
  tt <- tl(year)
  tt$d[1] <- 0
  month <- cut(timedays, c(tt$d, 365), labels=row.names(tt))
  as.numeric(month)
}
