\name{se}
\alias{se}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function to calculate standard error}
\description{
  This function computes the standard error of the values in x.
}
\usage{
se(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a numeric vector}
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
\seealso{See Also as \code{\link{sd}}, \code{\link{var}}}
\examples{
se(rnorm(100))

## The function is currently defined as
#function (x) 
#{
#    x <- x[!is.na(x)]
#    sqrt(var(x)/length(x))
#  }
#}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{standard error}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
