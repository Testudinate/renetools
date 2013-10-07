\name{matrev}
\alias{matrev}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function to reverse order of columns or rows of a matrix}
\description{
  Reverse order of columns or rows of a matrix.}
\usage{
matrev(mat, method = "cols")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mat}{a matrix or data.frame}
  \item{method}{can be either of \code{"cols"} or \code{"rows"}}
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
%\seealso{~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
a <- matrix(1:100, nrow=10)
a
matrev(a, method="rows")
matrev(a, method="cols")

## The function is currently defined as
#function (mat, method = "cols") 
#{
#    if (method == "cols") {
#        cols <- ncol(mat)
#        mat <- mat[, cols:1]
#    }
#    else {
#        if (method == "rows") {
#            rows <- nrow(mat)
#            mat <- mat[rows:1, ]
#        }
#        else {
#            stop("method unknown")
#        }
#    }
#    mat
#  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
