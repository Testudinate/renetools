\name{getdocs}
\alias{getdocs}
\title{Function to list all files with specific file extension}
\description{
Lists all files with a specific file extension in a given directory.
}
\usage{
getdocs(pfadr, pattern = "\\\\.TXT")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pfadr}{path to directory to be searched in}
  \item{pattern}{Only file names which match the regular expression will be returned.}
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
\seealso{See Also \code{\link{getdirs}}, \code{\link{dir}}}
\examples{
\dontrun{getdirs("c:/Dokumente/", pattern="\\.DOC")}

## The function is currently defined as
#function (pfadr, pattern = "\\.TXT") 
#{
#    files <- dir(pfadr, pattern = pattern)
#    files
#  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
