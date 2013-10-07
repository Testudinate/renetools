\name{wwctap}
\alias{wwctap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function for determination of WWC-Phases
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
wwctap(dat, lim = c(61, 4, 14, 20))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
%%     ~~Describe \code{dat} here~~
}
  \item{lim}{
%%     ~~Describe \code{lim} here~~
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
function (dat, lim = c(61, 4, 14, 20)) 
{
    nn <- nrow(dat)
    res <- data.frame(year = dat[, 1], Phase1 = NA, Phase2 = NA, 
        Phase3 = NA, Phase4 = NA)
    for (i in 1:nn) {
        if (is.na(dat[i, 2])) {
            res[i, 2] <- NA
        }
        else {
            if (dat[i, 2] < lim[1]) {
                res[i, 2] <- 1
            }
            else {
                res[i, 2] <- 0
            }
        }
        if (is.na(dat[i, 3])) {
            res[i, 3] <- NA
        }
        else {
            if (dat[i, 3] > lim[2]) {
                res[i, 3] <- 1
            }
            else {
                res[i, 3] <- 0
            }
        }
        if (is.na(dat[i, 4])) {
            res[i, 4] <- NA
        }
        else {
            if (dat[i, 4] > lim[3]) {
                res[i, 4] <- 1
            }
            else {
                res[i, 4] <- 0
            }
        }
        if (is.na(dat[i, 5])) {
            res[i, 5] <- NA
        }
        else {
            if (dat[i, 5] > lim[4]) {
                res[i, 5] <- 1
            }
            else {
                res[i, 5] <- 0
            }
        }
    }
    res
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
