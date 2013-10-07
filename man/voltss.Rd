\name{voltss}
\alias{voltss}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Hypsographical function of the Saidenbach Reservoir}
\description{
  Calculates Volume of the Saidenbach Reservoir at a given depth.
}
\usage{
voltss(z, sp = 438.8)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{z}{depth in m }
  \item{sp}{water level in m above Sealevel}
}
%\details{
%  ~~ If necessary, more details than the description above ~~
%}
\value{
Returns Volume in m^3.
}
%\references{ ~put references to the literature/web site here ~ }
\author{René Sachse}
%\note{ ~~further notes~~ 
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{See Also \code{\link{depthtss}}}
\examples{
Vmax <- voltss(0, sp=438.8)
Vmin <- voltss(46.1833, sp=438.8)

## The function is currently defined as
#function (z, sp = 438.8) 
#{
#    Vzmax <- 46.1833
#    zz <- z + 438.8 - sp
#    Vo <- 10^6 * 22.364
#    Vo * (1 - zz/Vzmax)^3
#  }
}
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
