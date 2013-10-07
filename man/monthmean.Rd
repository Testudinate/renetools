\name{monthmean}
\alias{monthmean}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function for extracting monthly means of a long term data set.}
\description{
  This function calculates monthly means of a variable of interest from a 
  long term data set.
}
\usage{
monthmean(dat, y="tair_calc")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{data.frame which needs the columns "year", "month" and at least 
    one variable}
  \item{y}{character; column name of the variable of interest}
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
    month=rep(rep(1:12, each=4), 3), 
    year=rep(2000:2002, each=4*12), 
    y=c(rnorm(12*4,2,1), rnorm(12*4,3,1), rnorm(12*4,4,1))
  )
  monthmean(dat, y="y")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{monthly mean, long term data}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
