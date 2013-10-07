fishers <-
function(alldat){
  ## just copied and unchecked code of Thomas Petzoldt for contingency table statistics
  lakes <- unique(alldat$lake)
  for (ll in lakes) {
    tss <- subset(alldat, lake=="Saidenbach")
    tsc <- subset(alldat, lake==ll)
    
    tm <- tss$time[tss$time %in% tsc$time]
    
    tss <- subset(tss, time %in% tm)[, 3:6]
    tsc <- subset(tsc, time %in% tm)[, 3:6]
    
    tss <- as.vector(as.matrix(tss))
    tsc <- as.vector(as.matrix(tsc))
    
    isna <- is.na(tsc)
    tss <- tss[!isna]
    tsc <- tsc[!isna]
    
    phi <- cor.test(as.vector(as.matrix(tsc)), as.vector(as.matrix(tss)))
    
    ct <- sum((tsc == tss) & (tss == 0))
    wt <- sum((tsc == tss) & (tss == 1))
    cf <- sum((tsc != tss) & (tss == 0))
    wf <- sum((tsc != tss) & (tss == 1))
    
    odds <- ct*wt/(cf*wf)
    #print(ll)
    conti <- matrix(c(ct, cf, wf, wt), nrow=2, byrow=TRUE)
    fish <- fisher.test(conti, sim=TRUE)
    chi  <- chisq.test(conti, correct=FALSE)
    #print(chi)
    est <- chi$statistic
    p   <- chi$p.value
    percentagree <- (ct + wt)/(ct + wt + cf + wf)
    cat(ll, ct, wt, cf, wf, est, p, "\t phi:", phi$estimate, phi$p.value, 
       "\t fish:", fish$estimate, fish$p.value, "vcd-oddsratio", oddsratio(conti, log=FALSE), odds, "?bereinstimmung = ", percentagree,  "\n")
  }
}
