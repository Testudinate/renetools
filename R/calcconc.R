calcconc <-
function(cm, Qm, Qd, ncl=15){
  cl <- loadharned(cm, Qm, Qd, ncl=ncl)
  Qdlog <- log(Qd)
  klassen  <- unique(c(cl[,2],cl[,3]))
  Qklassen <- cut(Qdlog, breaks=klassen, labels=FALSE, include.lowest=TRUE)
  ccalc    <- cl$cmean[Qklassen]
  ccalc
}
