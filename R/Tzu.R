################################################
### Lothars Modell                           ###
### zur Berechnung der                       ###
### Zuflusstemperatur (Tzu) aus der          ###
### Luftemperatur (tl)                       ###
### b1..b6 : Modellparameter                 ###
### m      : Anzahl der Tage mit R?ckwirkung ###
### doy    : day of the year                 ###
### tl     : Lufttemperatur                  ###
################################################

Tzu <- function(b1, b2, b3, b4, b5, b6, m, doy, tl){
  if(length(doy) != length(tl)){stop("doy and tl not of same length")}
  # Unterfunktion f?r X
  X <- function(d, b6){
    sin(2 * pi * (d + b6) / 365)
  }
  # Unterfunktion f?r TL
  TL <- function(tluft){
    1/length(tluft) * sum(tluft)
  }
  # Berechunung der Wassertemperaturen aus Lufttemperaturen f?r ganzen tl-Vektor
  tw <- NULL
  tw[1:(m-1)] <- NA
  for(i in (m:length(tl))){
    #print(i)
    d <- doy[i]
    tluft <- tl[(i-m+1):i]
    tw[i] <- b1 + b2*X(d, b6) + b3*X(d, b6)^2 + TL(tluft) * (b4 + b5*X(d, b6))
  }
  tw
}
