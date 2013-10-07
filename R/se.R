se <-
function(x){
  x <- x[!is.na(x)]
  sqrt(var(x)/length(x))
}
