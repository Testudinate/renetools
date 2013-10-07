\name{approx1Apr}
\alias{approx1Apr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to approximate a value for April 1st from a time series
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
approx1Apr(T10m, what = "T10m")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{T10m}{
%%     ~~Describe \code{T10m} here~~
}
  \item{what}{
%%     ~~Describe \code{what} here~~
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
function (T10m, what = "T10m") 
{
    res <- NULL
    jahre <- unique(T10m$year)
    for (i in 1:length(jahre)) {
        tdat <- subset(T10m, year == jahre[i])
        res[i] <- approx(tdat$doy, tdat[, what], tl(jahre[i])$d[4])$y
    }
    data.frame(year = jahre, T10m_1Apr = res)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
