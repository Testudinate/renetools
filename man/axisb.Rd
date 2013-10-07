\name{axisb}
\alias{axisb}
\title{Function to add an axis to plots with the labels between the tick marks}
\description{
  Adds an axis to plots with the labels between the tick marks.
}
\usage{
axisb(side = 1, at, labels, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{side}{specifies at which side of the plot the axis is to be drawn 
    (bottom=1, left=2, top=3, right=4)
  }
  \item{at}{the points at which tick-marks are to be drawn. 
    For details see \code{\link{axis}}}
  \item{labels}{a character or expression vector of labels to be placed at 
    the tickpoints. Needs to be of length: \code{length(at)-1}}
  \item{\dots}{are passed to \code{\link{axis}}}
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
\seealso{See Also \code{\link{axis}}}
\examples{

plot(
  seq(1.5,12.5,1), 
  rnorm(12),
  axes=FALSE,
  frame.plot=TRUE,
  type="h",
  lwd=5,
  xlab="id",
  ylab="random number"
)
axis(2)
axisb(1,1:13,1:12)


## The function is currently defined as
#function (side = 1, at, labels, ...) 
#{
#    axis(side, at = at, labels = FALSE, ...)
#    nat <- length(at)
#    atlab <- (at[1:(nat - 1)] + at[2:nat])/2
#    axis(side, at = atlab, labels = labels, tick = FALSE, ...)
#  }
#}
\keyword{axis}
