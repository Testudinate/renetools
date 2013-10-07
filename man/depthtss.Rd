\name{depthtss}
\alias{depthtss}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function to calculate depth of the Saidenbach Reservoir at given Volume}
\description{
  Calculates depth of the Saidenbach Reservoir at a given Volume.
}
\usage{
depthtss(V)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{V}{Volume in m^3}
}
%\details{
%  ~~ If necessary, more details than the description above ~~
%}
\value{
Returns a numerical value \code{z} which is the depth [m] for given Volume \code{V}.
The depth is relative to maximum water level of the Saidenbach Reservoir. See 
\code{\link{voltss}}.
}
%\references{ ~put references to the literature/web site here ~ }
\author{René Sachse}
%\note{ ~~further notes~~ 
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{See Also \code{\link{voltss}}}
\examples{  
## The function is currently defined as

## reverse hypsographic function
#depthtss <- function(V){
#  xnew <- seq(0,46,0.001)
#  voltssrev  <- approxfun(voltss(xnew),xnew)
#  z        <- voltssrev(V)
#  z
#}
}

\keyword{TSS}
\keyword{Saidenbach}
\keyword{hypsographic curve}