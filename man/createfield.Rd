\name{createfield}
\alias{createfield}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function to transform x, y and z values to 2D-Matrix}
\description{
A lighter and not so fancy clone of original \code{\link{mach.feld}} from Susanne 
Rolinski. A function which will create a Matrix of a data.frame giving x, y and z
values.
}
\usage{
createfield(dat, approxmethod = "constant", approxside = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{data.frame containing columns: \code{x, y, z}}
  \item{approxmethod}{\code{constant} or \code{linear}}
  \item{approxside}{For method="constant" a number between 0 and 1 inclusive, 
   indicating a compromise between left- and right-continuous step functions. 
   If \code{y0} and \code{y1} are the values to the left and right of the point 
   then the value is \code{y0*(1-f)+y1*f} so that \code{f=0} is right-continuous 
   and \code{f=1} is left-continuous.}
}
%\details{
%  ~~ If necessary, more details than the description above ~~
%}
%\value{
%  ~Describe the value returned
%  If it is a LIST, use
%  \item{comp1 }{Description of 'comp1'}
%  \item{comp2 }{Description of 'comp2'}
%  ...
%}
%\references{ ~put references to the literature/web site here ~ }
\author{René Sachse}
%\note{ ~~further notes~~ 
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{See Also \code{\link{mach.feld}}}
\examples{
x <- rep(1:10,10)
y <- rep(1:10,each=10)
dat <- data.frame(
  x=x, 
  y=y, 
  z= 3 * x * y + rnorm(100) * x * y
) 
mat <- createfield(dat)
image(mat)
filled.contour(mat, color.palette= rainbow)
require(lattice)
wireframe(mat, drape=TRUE)

## The function is currently defined as
#function (dat, approxmethod = "constant", approxside = 0) 
#{
#    require(reshape)
#    mdat <- melt(dat, id = c("x", "y"), measure = "z")
#    cdat <- cast(mdat, y ~ x, fun.aggregate = mean)
#    ccdat <- as.matrix(cdat)
#    for (i in 1:ncol(ccdat)) {
#        x <- as.numeric(row.names(ccdat))
#        y <- ccdat[, i]
#        xnew <- x
#        nna <- length(which(!is.na(ccdat[, i])))
#        if (nna > 1) {
#            ynew <- approx(x, y, xnew, method = approxmethod, 
#                f = approxside)$y
#            ccdat[, i] <- ynew
#        }
#    }
#    ccdat
#  }
}
\keyword{filled.contour}
\keyword{Slcontour}
\keyword{contour}
\keyword{contourplot}