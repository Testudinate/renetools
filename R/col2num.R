col2num <-
function(dat, cols=c(2,3,5,12,13)){
  for(i in cols){
    dat[,i] <- as.character(dat[,i])
    dat[,i] <- as.numeric(dat[,i])                  # char -> numeric
  }
  dat
}
