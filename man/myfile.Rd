\name{myfile}
\alias{myfile}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function to connect a path consisting of two parts.}
\description{
  Allows to connect two parts of a path to a single one, e.g. a path and a filename.
}
\usage{
myfile(path, file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path}{first part of the path}
  \item{file}{second part of the path}
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
\seealso{See Also \code{\link{paste}}}
\examples{
path1 <- "c:/Dokumente/"
file <- "readme.doc"
path <- myfile(path1, file)

## The function is currently defined as
#function (path, file) 
#{
#    out <- paste(path, file, sep = "")
#    out
#  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
