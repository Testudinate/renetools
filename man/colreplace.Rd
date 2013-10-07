\name{colreplace}
\alias{colreplace}
\title{Function to find and replace specified values in a data frame.}
\description{
The function finds a given value in specified columns of a data frame and
replaces them by another given value.
}
\usage{
colreplace(dat, find = 0, fill = NA, cols = 2:4)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{data frame}
  \item{find}{character or numeric to be found}
  \item{fill}{character or numeric used for replacement}
  \item{cols}{columns of the data frame in which pattern is to be replaced}
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
\seealso{See Also as \code{\link{na2zero}}}
\examples{
dat <- data.frame(x=1:10, y=c(5,5,5,1,5,5,5,1,1,8))
dat <- colreplace(dat, find=1, fill=5, cols=2)

## The function is currently defined as
#function (dat, find = 0, fill = NA, cols = 2:4) 
#{
#    for (i in cols) {
#        dat[, i] <- ifelse(dat[, i] == find, fill, dat[, i])
#    }
#    dat
#  }
}
\keyword{replace}
