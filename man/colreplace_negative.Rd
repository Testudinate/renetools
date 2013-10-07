\name{colreplace_negative}
\alias{colreplace_negative}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to find and replace negative values in a data frame or matrix
}
\description{
The function finds negative values in specified columns of a data frame or a matrix and replaces them by another given value.
}
\usage{
colreplace_negative(dat, fill = 0, cols = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
data frame or matrix
}
  \item{fill}{
character or numeric used for replacement
}
  \item{cols}{
coluns of the data frame in which negative values are to be replaced
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Rene Sachse
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
dat <- matrix(rnorm(100), nrow=10)
dat
dat <- colreplace_negative(dat)
dat
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
