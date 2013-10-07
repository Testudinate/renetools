colreplace_negative <-
function (dat, fill = 0, cols = NULL) 
{
  if(is.null(cols)){
    cols <- 1:ncol(dat)
  }
  for (i in cols) {
    dat[, i] <- ifelse(dat[, i] < 0, fill, dat[, i])
  }
  dat
}
