\name{col2char}
\alias{col2char}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function to convert columns to character}
\description{
Converts the type of specified columns of a data frame to character.
}
\usage{
col2char(dat, cols = c(2, 3, 5, 12, 13))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{a data frame}
  \item{cols}{columns which are to be converted to character}
}
\details{
  uses \code{\link{as.character}}
}
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
\seealso{See Also \code{\link{as.character}}}
\examples{

dat <- data.frame(x = as.factor(c("a", "b", "c")), y=1:3)
str(dat) #1st column is of type \code{levels}
dat <- col2char(dat, 1)
str(dat) #1st column is of type \code{character}

## The function is currently defined as
#function (dat, cols = c(2, 3, 5, 12, 13)) 
#{
#    for (i in cols) {
#        dat[, i] <- as.character(dat[, i])
#    }
#    dat
#  }
}
\keyword{character}
