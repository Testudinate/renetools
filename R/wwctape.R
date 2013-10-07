wwctape <-
function(
  dat,
  horiz=FALSE,
  cold="blue", warm="red", nacol="black",
  textwarm="black", textcold="black",
  lim=c(61,4,14,20), ...
){
  nn <- nrow(dat)
  xx <- 1:nn
  yy <- 1:4
  xticks <- dat[xx, 1]
  yticks <- c("I", "II", "III", "IV")
  ylim <- c(max(xx)+1, min(xx))
  las  <- 1
  xpos <- 3
  if(horiz){
    temp   <- xx
    xx     <- yy
    yy     <- temp
    temp   <- xticks
    xticks <- yticks
    yticks <- temp
    ylim   <- c(min(xx), max(xx+1))
    las    <- 2
    xpos   <- 1
  }
  plot(NA, axes=FALSE, xlim=c(min(yy), max(yy)+1), ylim = ylim, xlab="", ylab="", ...)
  for(i in 1:nn){
    if(is.na(dat[i,2])){
     col1 <- nacol
     t1c  <- nacol
     t1   <- ""
    } else {
     if(dat[i,2] < lim[1]){
        col1 <- warm
        t1c  <- textwarm
        t1   <- "w"
      } else {
        col1 <- cold
        t1c  <- textcold
        t1   <- "c"
      }
    }
    if(is.na(dat[i,3])){
     col2 <- nacol
     t2c  <- nacol
     t2   <- ""
    } else {
      if(dat[i,3] > lim[2]){
        col2 <- warm
        t2c  <- textwarm
        t2   <- "w"
      } else {
        col2 <- cold
        t2c  <- textcold
        t2   <- "c"
      }
    }
    if(is.na(dat[i,4])){
     col3 <- nacol
     t3c  <- nacol
     t3   <- ""
    } else {
      if(dat[i,4] > lim[3]){
        col3 <- warm
        t3c  <- textwarm
        t3   <- "w"
      } else {
        col3 <- cold
        t3c  <- textcold
        t3   <- "c"
      }
    }
    if(is.na(dat[i,5])){
     col4 <- nacol
     t4c  <- nacol
     t4   <- ""
    } else {
      if(dat[i,5] > lim[4]){
        col4 <- warm
        t4c  <- textwarm
        t4   <- "w"
      } else {
        col4 <- cold
        t4c  <- textcold
        t4   <- "c"
      }
    }
    if(horiz){
      rect(i, 1:4, i+1, 2:5, col=c(col1, col2, col3, col4), border=NA)
      text(i + 0.5, c(1.5, 2.5, 3.5, 4.5), c(t1, t2, t3, t4), col=c(t1c, t2c, t3c, t4c), font=2)
    } else {
      rect(1:4, i, 2:5, i+1, col=c(col1, col2, col3, col4), border=NA)
      text(c(1.5, 2.5, 3.5, 4.5), i+0.5, c(t1, t2, t3, t4), col=c(t1c, t2c, t3c, t4c), font=2)
     }
  }
  axis(2, xx + 0.5, xticks, las=las, line=-1, tick=FALSE)
  axis(xpos, yy + 0.5, yticks, las=las, line=-1, tick=FALSE)
}
