mach.feld <-
function(tabi,dep=NULL,unten.glatt=FALSE) {
 xg <- sort(unique(tabi[,1]))
 if (length(dep)<1)  yg <- sort(na.omit(unique(tabi[,2]))) else yg <- dep
 im  <- matrix(-1,nrow=length(yg),ncol=length(xg))
 nna <- NULL
 for (n in 1:length(xg)) {
   itab <- which(tabi[,1] == xg[n])
   if (length(!is.na(tabi[itab,3]))<2) nna <- c(nna,n)
   else im[,n] <- approx(tabi[itab,2],tabi[itab,3],yg,rule=1)$y
 }
 if (length(nna) > 0) {
   xg <- xg[-nna]
   im <- im[,-nna]
 }
 if (unten.glatt) {
   li  <- nrow(im)
   hli <- floor(0.7*li)
   for (n in 1:length(xg)) {
     if (is.na(im[li,n])) {
       fli <- hli-1+which(is.na(im[hli:li,n]))[1]
       im[hli:li,n] <- approx(yg[hli:(fli-1)],im[hli:(fli-1),n],yg[hli:li],rule=2)$y
     }
   }
 }
 list(x=xg,y=yg,z=im)
}
