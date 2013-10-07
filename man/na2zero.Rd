\name{na2zero}
\alias{na2zero}

\title{Function to replace NAs in data.frames by 0}
\description{
  Replaces any NA in a data.frame by 0 or any other given value.
}
\usage{
na2zero(dat, fill = 0)
}

\arguments{
  \item{dat}{data.frame}
  \item{fill}{character or numerical replacement for the NAs}
}

\seealso{See Also \code{\link{colreplace}}}
\examples{
dat <- data.frame(x=c(1,NA,5,NA), y=c(NA,3,4,5))
dat <- na2zero(dat)
dat

## The function is currently defined as
#function (dat, fill = 0) 
#{
#    cols <- ncol(dat)
#    rows <- nrow(dat)
#    for (i in 1:cols) {
#        dat[, i] <- ifelse(is.na(dat[, i]), fill, dat[, i])
#    }
#    dat
#  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
