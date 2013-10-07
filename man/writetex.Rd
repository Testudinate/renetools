\name{writetex}
\alias{writetex}
\title{Export of data.frames to LaTeX-Files}
\description{
  Formats a data.frame to valid LaTeX-Code and writes to a file.
}
\usage{
 writetex(dat, file, digits=2)
}
\arguments{
  \item{dat}{data.frame or any object accepted by xtable}
  \item{file}{character of the file to which to write}
  \item{digits}{numeric specifing the numbers of digits to be shown}
}
\author{René Sachse}
\seealso{See Also \code{\link{xtable}}}
\keyword{LaTeX, table}
