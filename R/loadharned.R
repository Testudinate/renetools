loadharned <-
function(c, Q, Qy, ncl=15, logclass=TRUE) {  #c=konz=>bdat$Ha; Q sind die Qms, wo es Konzentrationen zu gibt =>bdat$Qm, Qy = alle Qms =>vers$Qm
  ## log oder kein log
  trans <- function(Q, logclass) {
    if (logclass) Q <-log(Q)
    Q
  }
  ## Teil 2: Klasseneinteilung und zusammenfassen der Klassen
  ## 2.1: provisorische Klassengrenzen
  minQ <- min(trans(Qy, logclass))
  maxQ <- max(trans(Qy, logclass))

  cl   <- seq(minQ, maxQ, length=ncl+1)
  qcl  <- cut(trans(Qy, logclass), breaks=cl, labels=FALSE, include.lowest=TRUE)
  tqcl <- as.data.frame(table(qcl))
  xxx  <- data.frame(no = 1:ncl,
                     ll = cl[-(ncl+1)],
                     ul = cl[-1]
                    )

  sample.qcl  <- cut(trans(Q, logclass), breaks=cl, labels=FALSE, include.lowest=TRUE)
  sample.tqcl <- as.data.frame(table(sample.qcl))
  xxx$f <- tqcl$Freq[match(1:ncl, sample.tqcl$sample.qcl)]
  xxx$f[is.na(xxx$f)] <- 0
  unwichtig <- unique(c(xxx$ll[xxx$f==0], xxx$ul[xxx$f==0]))

  ## 2.2: neue Klassengrenzen
  cl <- cl[!(1:(ncl+1) %in% match(unwichtig,cl))]

  ## A1: erweitern bis zu Randklassen
  ## A2: waere Teilung einer Klasse vor Erweiterung
  cl  <- unique(c(minQ, cl, maxQ))
  ncl <- length(cl)-1
  ## Teil 3: Konzentrationen zuordnen
  qcl  <- cut(trans(Qy, logclass), breaks=cl, labels=FALSE, include.lowest=TRUE)    #cut sucht, in welcher Klasse Q drinliegt
  tqcl <- as.data.frame(table(qcl))
  xcl  <- data.frame(no = 1:ncl,
                     ll = cl[-(ncl+1)],
                     ul = cl[-1]
                     )

  xcl$cmean <- as.numeric(lapply(split(c, cut(trans(Q, logclass), breaks=cl, labels=FALSE, include.lowest=TRUE)), mean))
  xcl
}
