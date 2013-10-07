\name{eb}
\alias{eb}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function to draw error bars}
\description{
  Adds error bars to a plot.
}
\usage{
eb(xx, yy, ee, ew = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xx}{numerical vector of x-values where error bars are to be drawn}
  \item{yy}{numerical vector of y-values where error bars are to be drawn.}
  \item{ee}{numerical vector of the errors}
  \item{ew}{width of the whiskers, if \code{ew = NULL} width is 1/200 of \code{max(x)}}
  \item{\dots}{passed to \code{lines()}}
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

% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{See Also \code{\link{sd}}, \code{\link{se}}}
\examples{
dat <- data.frame(
  x = rep(1:10,10),
  y = rnorm(100) + 0.8 * rep(1:10,10)
)
datag <- aggregate(list(mean=dat$y), list(x=dat$x), mean)
datsd <- aggregate(list(sd=dat$y), list(x=dat$x), sd)
datse <- aggregate(list(se=dat$y), list(x=dat$x), se)
par(mfrow=c(1,2))
plot(datag$x, datag$mean, pch=16, main="Standard Error")
eb(datag$x, datag$mean, datse$se)
plot(datag$x, datag$mean, pch=16, main="Standard Deviation")
eb(datag$x, datag$mean, datsd$sd)

## The function is currently defined as
#function (xx, yy, ee, ew = NULL, ...) 
#{
#    if (is.null(ew)) {
#        ew <- max(xx, na.rm = TRUE)/200
#    }
#    for (i in 1:length(xx)) {
#        x <- xx[i]
#        y <- yy[i]
#        e <- ee[i]
#        lines(c(x, x), c(y - e, y + e), ...)
#        lines(c(x - ew, x + ew), c(y + e, y + e), ...)
#        lines(c(x - ew, x + ew), c(y - e, y - e), ...)
#    }
#  }
}
\keyword{error bars}
