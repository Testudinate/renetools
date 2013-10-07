sigstars <-
function(dat, p1=0.05, p2=0.01, p3=0.001){
  sD <- rep("", length(dat))
  sD <- ifelse(dat < p1, "*", sD)
  sD <- ifelse(dat < p2, "**", sD)
  sD <- ifelse(dat < p3, "***", sD)
  sD
}
