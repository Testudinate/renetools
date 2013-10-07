\name{getdirs}
\alias{getdirs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function to list all subdirectories below a given directory}
\description{
  Lists all subdirectories below a given directory. Files are not shown as long
  as they have a file extension seperated by ".". 
}
\usage{
getdirs(pfadr)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pfadr}{path of directory to be searched in}
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
\seealso{See Also \code{\link{getdocs}}, \code{\link{dir}}}
\examples{
\dontrun{getdirs("c:/")}

## The function is currently defined as
#function (pfadr) 
#{
#    dirs <- dir(pfadr)
#    files <- dir(pfadr, pattern = "\\.")
#    idx <- match(files, dirs)
#    dirs <- dirs[-idx]
#    dirs
#  }
#}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
