\name{loadharned}
\alias{loadharned}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function to classify discharge events and measured substance concentrations.}
\description{
  This function classifies discharge and given concentrations according to Harned et al (1981).
}
\usage{
loadharned(c, Q, Qy, ncl=15, logclass=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{c}{vector of measured concentrations}
  \item{Q}{vector of corresponding discharge}
  \item{Qy}{vector of discharge for which the class should be computed}
  \item{ncl}{numeric, maximum number of classes}
  \item{logclass}{logical, whether or not lograthmic classes are wanted}
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
%\seealso{See Also as \code{\link{sd}}, \code{\link{var}}}
\references{
  Harned, D. A., Daniel III, C. C. \& Crawford, J. K. (1981). 
  Methods of discharge compensation as an aid to the evaluation of water 
  quality trends. Water Resources Research, 17(5), 1389-1400.
}
\examples{
  cm <- rnorm(15, 5, 2)   #measured concentrations
  Qm <- 1:15              #corresponding measured daily discharge
  Qd <- 1:30              #daily discharges for which concentration should be calculated
  loadharned(cm, Qm, Qd)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{load, loadings, discharge, inflow, Harned}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
