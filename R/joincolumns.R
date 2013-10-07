joincolumns <-
function(dat1, dat2, common1, common2, what1, what2){
  id <- match(dat1[,common1], dat2[,common2])
  dat1[,what1] <- dat2[id,what2]
  dat1
}
