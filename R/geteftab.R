geteftab <-
function(ef){
  a <- ef$vectors$arrows
  a <- as.data.frame(a)
  a$r2 <- ef$vectors$r
  a$p  <- ef$vectors$pvals
  a$sig <- NA
  a$sig <- ifelse(a$p <= 0.1, ".", a$sig)
  a$sig <- ifelse(a$p <= 0.05, "*", a$sig)
  a$sig <- ifelse(a$p <= 0.01, "**", a$sig)
  a$sig <- ifelse(a$p <= 0.001, "***", a$sig)
  a
}
