\name{approxtw}
\alias{approxtw}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function for weigthed approximation}
\description{
  This function approximates in a way that missing values are filled by a weight
  of 0.5 by surrounding values}
\usage{
approxtw(x, y, xnew)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a numeric vector, independent variable}
  \item{y}{a numeric vector, dependent variable}
  \item{xnew}{a numeric vector providing new sampling points}
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
x <- c(2,4,11)
y <- c(1,2,3)
xnew <- 1:15
approxtw(x, y, xnew)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{approx}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
