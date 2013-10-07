monthtwmean <-
function (dat, column = "bm")
  {
    years <- unique(dat$year)
    erg <- NULL
    for (i in 1:length(years)) {
      tdat <- subset(dat, year == years[i])
      doys <- tdat$doy
      doynew <- seq(min(tdat$doy), max(tdat$doy), 1)
      #doynew <- seq(min(tdat$doy), lastdoy(years[i]), 1)
      tnew  <- rep(NA, lastdoy(years[i]))
      if (nrow(tdat) > 1) {
        tnew[1:max(tdat$doy)] <- approxtw(tdat$doy, tdat[, column], doynew)
      }
      else {
        tnew <- rep(NA, length(doynew))
      }
      mb <- c(tl(years[i])$d, lastdoy(years[i]))
      mu <- mb[1:12]
      mo <- c(mb[2:12] - 1, mb[13])
      mm <- NULL
      for (m in 1:12) {
        mm[m] <- mean(tnew[mu[m]:mo[m]], na.rm = TRUE)
      }
      erg <- rbind(erg, mm)
    }
    row.names(erg) <- years
    colnames(erg) <- tl(2010)$m
    erg <- colreplace(erg, "NaN", NA, cols = 1:12)
    erg
  }
