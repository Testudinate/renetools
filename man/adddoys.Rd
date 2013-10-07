\name{adddoys}
\alias{adddoys}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function to add column with day of the year, month and year.
}
\description{
Function to add column with day of the year, month and year to a data frame 
already containing a column with full POSIX-date}
\usage{
adddoys(dat, col = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{data frame containing a column with POSIX-date}
  \item{col}{number of the column containing the POSIX-date }
}
%\details{
%  
%}
\value{
  The colums \code{dat$doy}, \code{dat$month} and \code{dat$year} will be added
  to the data.frame
}
%\references{ ~put references to the literature/web site here ~ }
\author{René Sachse}
%\note{ ~~further notes~~ 
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{See Also \code{\link{datex}}}
\examples{

dat <- data.frame(
  date = as.POSIXct(c("2009-01-01","2009-05-25","2010-07-08")),
  x    = 1:3
)
dat <- adddoys(dat)

## The function is currently defined as
#function (dat, col = 1) 
#{
#    dat$doy <- datex(dat[, col], "\%j")
#    dat$month <- datex(dat[, col], "\%m")
#    dat$year <- datex(dat[, col], "\%Y")
#    dat
#  }
#}
\keyword{date}
\keyword{POSIX}
\keyword{doy}
\keyword{format}