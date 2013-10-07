\name{fishers}
\alias{fishers}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function which runs several contingency tests for 4-field-tables
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fishers(alldat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{alldat}{
%%     ~~Describe \code{alldat} here~~
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
function (alldat) 
{
    lakes <- unique(alldat$lake)
    for (ll in lakes) {
        tss <- subset(alldat, lake == "Saidenbach")
        tsc <- subset(alldat, lake == ll)
        tm <- tss$time[tss$time \%in\% tsc$time]
        tss <- subset(tss, time \%in\% tm)[, 3:6]
        tsc <- subset(tsc, time \%in\% tm)[, 3:6]
        tss <- as.vector(as.matrix(tss))
        tsc <- as.vector(as.matrix(tsc))
        isna <- is.na(tsc)
        tss <- tss[!isna]
        tsc <- tsc[!isna]
        phi <- cor.test(as.vector(as.matrix(tsc)), as.vector(as.matrix(tss)))
        ct <- sum((tsc == tss) & (tss == 0))
        wt <- sum((tsc == tss) & (tss == 1))
        cf <- sum((tsc != tss) & (tss == 0))
        wf <- sum((tsc != tss) & (tss == 1))
        odds <- ct * wt/(cf * wf)
        conti <- matrix(c(ct, cf, wf, wt), nrow = 2, byrow = TRUE)
        fish <- fisher.test(conti, sim = TRUE)
        chi <- chisq.test(conti, correct = FALSE)
        est <- chi$statistic
        p <- chi$p.value
        percentagree <- (ct + wt)/(ct + wt + cf + wf)
        cat(ll, ct, wt, cf, wf, est, p, "\t phi:", phi$estimate, 
            phi$p.value, "\t fish:", fish$estimate, fish$p.value, 
            "vcd-oddsratio", oddsratio(conti, log = FALSE), odds, 
            "?bereinstimmung = ", percentagree, "\n")
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
