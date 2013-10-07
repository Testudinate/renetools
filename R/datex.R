datex <-
function(x, how, num=TRUE){
  a <- format(x, how)
  if(num == TRUE){a <- as.numeric(a)}
  a
}
