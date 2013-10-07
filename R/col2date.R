col2date <-
function(dat, cols=c(1)){
  for(i in cols){
    dat[,i] <- as.POSIXct(as.character(dat[,i]))      # date as POSIX
  }
  dat
}
