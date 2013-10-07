\name{datex}
\alias{datex}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function to extract specific values from POSIX dates}
\description{
 The function helps to extract numerical values of "day of the year", "month" or 
 "year" from a given POSIX date.
}
\usage{
datex(x, how, num = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{an object to be converted}
  \item{how}{A character string. The default is \code{"\%Y-\%m-\%d \%H:\%M:\%S"} if any 
    component has a time component which is not midnight, and \code{"\%Y-\%m-\%d"} 
    otherwise}
  \item{num}{logical, wheter the result should be numerical or not}
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
%\references{}
\author{René Sachse}
%\note{ ~~further notes~~ 
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{See Also \code{\link{format.POSIXct}}}
\examples{
dat <- as.POSIXct("2009-12-24")
dayofyear <- datex(dat, "\%j")
month     <- datex(dat, "\%m")
year      <- datex(dat, "\%Y")

## The function is currently defined as
#function (x, how, num = TRUE) 
#{
#    a <- format(x, how)
#    if (num == TRUE) {
#        a <- as.numeric(a)
#    }
#    a
#  }
}
\keyword{POSIX}
\keyword{format}% __ONLY ONE__ keyword per line
