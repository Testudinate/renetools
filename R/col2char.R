col2char <-
function(dat, cols=c(2,3,5,12,13)){
  for(i in cols){
    dat[,i] <- as.character(dat[,i])                  # level -> string
  }
  dat
}
