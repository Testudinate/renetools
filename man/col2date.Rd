\name{col2date}
\alias{col2date}
\title{Function to convert a column to POSIX date}
\description{
Converts a column containing dates as characters to POSIX
}
\usage{
col2date(dat, cols = c(1))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{data frame}
  \item{cols}{number of column which is to be converted to POSIX}
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
\seealso{\code{\link{col2char}}}
\examples{

dat <- data.frame(date = c("2009-05-20 00:00:00","2009-12-24 23:59:59", "2010-10-10 10:10:10"), x=1:3)
str(dat) #1st column is factor
dat <- col2date(dat)
str(dat) #1st column is POSIX

## The function is currently defined as
#function (dat, cols = c(1)) 
#{
#    for (i in cols) {
#        dat[, i] <- as.POSIXct(as.character(dat[, i]))
#    }
#    dat
#  }
}
\keyword{POSIX}
