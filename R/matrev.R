matrev <-
function(mat, method="cols"){
  if(method=="cols"){
      cols <- ncol(mat)
      mat <- mat[,cols:1]
  } else {
    if(method=="rows"){
      rows <- nrow(mat)
      mat <- mat[rows:1,]
    } else {
      stop("method unknown")
    }
  }
  mat
}
