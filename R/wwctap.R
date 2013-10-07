wwctap <-
function(
  dat,
  lim=c(61,4,14,20)
){
  nn <- nrow(dat)
  res <- data.frame(year = dat[,1], Phase1=NA, Phase2=NA, Phase3=NA, Phase4=NA)
  for(i in 1:nn){
    if(is.na(dat[i,2])){
      res[i,2]  <-  NA
    }  else {
      if(dat[i,2] < lim[1]){res[i,2] <- 1} else {res[i,2] <- 0}
    }
    if(is.na(dat[i,3])){
      res[i,3]  <-  NA
    }  else {
      if(dat[i,3] > lim[2]){res[i,3] <- 1} else {res[i,3] <- 0}
    }
    if(is.na(dat[i,4])){
      res[i,4]  <-  NA
    }  else {
      if(dat[i,4] > lim[3]){res[i,4] <- 1} else {res[i,4] <- 0}
    }
    if(is.na(dat[i,5])){
      res[i,5]  <-  NA
    }  else {
      if(dat[i,5] > lim[4]){res[i,5] <- 1} else {res[i,5] <- 0}
    }
  }
  res
}
