\name{episum}
\alias{episum}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calculates epilimnetic and whole water column sums for profile data
%%  ~~function to do ... ~~
}
\description{Estimates mixing depth from temperature profiles and calculates epilimnetic and whole water column sums of a second variable. Temperature and data matrix need to be of the same dimensions. Columns correspond to the times vector (increasing), rows correspond to the depths vector. Depths are negative and begin with the deepest depth (-109, -108, -107,...).
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
epimean(times, depth, temp, dat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{times}{numeric, the time steps (increasing)
%%     ~~Describe \code{times} here~~
}
  \item{depth}{numeric, the depths. Need to be negative, start with highest depth.
%%     ~~Describe \code{depth} here~~
}
  \item{temp}{numeric, matrix providing temperature profiles. columns = times, rows = depths, with highest depth in first row.
%%     ~~Describe \code{temp} here~~
}
  \item{dat}{numeric, matrix providing profiles from which sums are to be calculated. columns = times, rows = depths, with highest depth in the top.
%%     ~~Describe \code{dat} here~~
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
data(time)
data(depth)
data(temp)
data(phy)

## temperature means
means <- episum(time, depth, temp, temp)
head(means)
plot(means$times/86400, means$sumepi, type="l")
points(means$times/86400, means$sum10, type="l", col="red")
points(means$times/86400, means$sumcolumn, type="l", col="blue")

## phytoplankton means
means <- episum(time, depth, temp, phy)
head(means)
plot(means$times/86400, means$sumepi, type="l")
points(means$times/86400, means$sum10, type="l", col="red")
points(means$times/86400, means$sumcolumn, type="l", col="blue")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ epilimnion }
\keyword{ mean }% __ONLY ONE__ keyword per line
