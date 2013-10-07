\name{getzmix}
\alias{getzmix}

\title{Calculation of mixing depth in TS Saidenbach according to
  Lothar Paul
}

\description{
  Calculation of mixing depth in TS Saidenbach according to Lothar Paul
}

\usage{
 getzmix(z, temp, z10=FALSE, thyp=10, approx=FALSE)
}

\arguments{
  \item{z}{numeric, vector of orderd depths}
  \item{temp}{numeric, vector of temperatures}
  \item{z10}{logical, whether 10m-isotherm should be returned}
  \item{thyp}{numeric, treshold temperature between meta- and
    hypolimnion}
  \item{approx}{logical, whether temperature profile should be
    axproximated to 0.1 m steps or not}
}

\value{
  numeric vector, first position is zmix, second value is z10 if computation was wished
}

\author{\enc{René}{Rene} Sachse}

%\seealso{See Also \code{\link{odbcConnectExcel}}}
\examples{
  z    <- 1:43
  temp <- c(
    17.1,17.1,17.1,16.9,16.5,16.3,15.5,14.5,11,8.8,7.3,6.8,5.6,5.4,
    5.1,5,4.7,4.7,4.6,4.6,4.5,4.5,4.4,4.4,4.4,4.4,4.3,4.2,4.2,4.2,
    4.1,4.1,4.1,4.1,4.1,4.1,4.2,4.2,4.2,4.2,4.2,4.2,4.2
  )
  zmix <- getzmix(z, temp, z10=TRUE, approx=TRUE)
  plot(temp,z,ylim=c(45,0), type="l", xlab="Temperature °C", ylab="Depth m", las=1)
  abline(h=zmix[1], col="red")
  abline(h=zmix[2], col="blue")
}
\keyword{mixing depth}
\keyword{zmix}
