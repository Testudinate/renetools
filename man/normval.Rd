\name{normval}
\alias{normval}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function for normalization and scaling}
\description{
  This function normalizes the values x to a lower and upper boundary.
}
\usage{
normval(x, normmin, normmax)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a numeric vector}
  \item{normmin}{lower value for normalization}
  \item{normmax}{upper value for normalization}
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
normval(0:10, -0.5, 0.5)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{normalization, normalisation, scaling}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
