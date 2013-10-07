approxtw <-
function(x,y,xnew){
  ldoy <- max(xnew)
  fdoy <- min(xnew)
  border <- (x[1:(length(x) - 1)] + floor(diff(x)/2))
  upper <- c(border, ldoy)
  lower <- c(fdoy, border + 1)
  ynew <- rep(NA, length(xnew))
  for (i in 1:length(upper)) {
      ynew[x[i]:upper[i]] <- y[i]
      ynew[lower[i]:x[i]] <- y[i]
  }
  mod2 <- diff(x)%%2 == 0
  for(i in 1:length(mod2)){
    if(mod2[i] == TRUE){ynew[border[i]] <- mean(ynew[c(border[i]-1, border[i]+1)])}
  }
  ynew
}
