\name{day2month}
\alias{day2month}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function which returns corresponding month for a given day of the year
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
day2month(timedays, year)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{timedays}{
%%     ~~Describe \code{timedays} here~~
}
  \item{year}{
%%     ~~Describe \code{year} here~~
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
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (timedays, year) 
{
    tt <- tl(year)
    tt$d[1] <- 0
    month <- cut(timedays, c(tt$d, 365), labels = row.names(tt))
    as.numeric(month)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
