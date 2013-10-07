na2zero <-
function(dat, fill=0){
  cols <- ncol(dat)
  rows <- nrow(dat)
  for(i in 1:cols){
    dat[,i] <- ifelse(is.na(dat[,i]), fill, dat[,i])
  }
  dat
}
