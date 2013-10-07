\name{sigstars}
\alias{sigstars}

\title{Drawing significance stars}
\description{
  Draws one, two or three significance stars according to given p-values.
}
\usage{
  sigstars(dat, p1=0.05, p2=0.01, p3=0.001)
}


\arguments{
  \item{dat}{numeric vector of p-values}
  \item{p1}{numeric, lowest significance level}
  \item{p2}{numeric, medium significance level}
  \item{p3}{numeric, highest significance level}
}

\author{René Sachse}

\examples{
dat <- data.frame(p=c(0.38,0.041, 0.003, 0.0006))
dat$sig <- sigstars(dat$p) 
}
\keyword{p, p-value, significance}
