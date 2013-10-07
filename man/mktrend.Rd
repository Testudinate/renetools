\name{mktrend}
\alias{mktrend}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function for computing Mann Kendall statistics and Sen's slope}
\description{
  This function calculates Mann Kendall statistics and Sen's slopes.
}
\usage{
mktrend(dat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{data.frame with 12 columns representing 12 months and every row 
  represents a year}
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
%\seealso{See Also as \code{\link{sd}}, \code{\link{var}}}
\examples{
  dat <- data.frame(
    doy=rep(tl(2010)$d[1:12],each=4) +  rep(c(0,10,20,25),12*6),
    month=rep(rep(1:12, each=4), 6),
    year=rep(2000:2005, each=4*12),
    y=c(rnorm(12*4,2,1), rnorm(12*4,3,1), rnorm(12*4,4,1), rnorm(12*4,5,1), rnorm(12*4,6,1), rnorm(12*4,7,1))
  )
  ym <- monthmean(dat, "y")
  mktrend(ym)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Mann Kendall, Sens slope}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
