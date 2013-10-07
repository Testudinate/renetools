colreplace <-
function(dat, find=0, fill=NA, cols=2:4){
  for(i in cols){
    dat[,i] <- ifelse(dat[,i] == find, fill, dat[,i])
  }
  dat
}
