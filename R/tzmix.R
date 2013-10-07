tzmix <-
function(t2m){
    sterm <- function(ai, bi, t2m){
      ai * exp(bi * t2m)
    }
    res <- ifelse(
      t2m >= 7.2 & t2m <= 30,
      t2m - sterm(0.28, 0, t2m) - sterm(3031200, -2, t2m) - sterm(5.2, -0.2, t2m),
      NA
    )
    res
}
