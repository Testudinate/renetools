\name{zeitmittel}
\alias{zeitmittel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function to calculate time weighted means.}
\description{
Calculates time weighted means for a given time series.
}
\usage{
zeitmittel(doy, temp, anfang, ende)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{doy}{day of the year}
  \item{temp}{the variable}
  \item{anfang}{first doy of period of interest}
  \item{ende}{last doy of period of interest}
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
\seealso{\code{\link{vertmeanown}}}
\examples{
x <- c(200, 205, 210, 230, 250)
y <- rnorm(5,mean=5)
zeitmittel(x,y,200,250)

## The function is currently defined as
#function (doy, temp, anfang, ende) 
#{
#    vertmeanown(doy, temp, anfang - 0.5, ende + 0.5)
#  }
#}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
