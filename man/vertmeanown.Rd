\name{vertmeanown}
\alias{vertmeanown}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function for weighted vertical means}
\description{
Calculates weighted vertical means. This is a more simple function than the one 
provided by package \code{limnotools}, \code{vertmean}.
}
\usage{
vertmeanown(depth, vari, top, bot)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{depth}{vector of depths}
  \item{vari}{vector of variable}
  \item{top}{top of the water body}
  \item{bot}{bottom of the water body}
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
%5 ~Make other sections like Warning with \section{Warning }{....} ~
%}
%\seealso{\code{\link{limnotools}}, \code{\link{vertmean}}}
\examples{
x <- c(2,4,6,10)
y <- c(30,55,20,2)
vertmeanown(x,y, top=0, bot=10) == (30*3 + 55*2 + 20*3 + 2*2)/10


## The function is currently defined as
#function (depth, vari, top, bot) 
#{
#    depthori <- depth
#    variori <- vari
#    if (bot < max(depth)) {
#        tdepth <- depth[depth <= bot]
#        tvari <- vari[depthori <= bot]
#        if (length(tdepth) < 1) {
#            tdepth <- bot
#            tvari <- vari[which(vari == min(vari))[1]]
#        }
#        depth <- tdepth
#        vari <- tvari
#    }
#    depthori2 <- depth
#    if (top > min(depth)) {
#        depth <- depth[depth >= top]
#        vari <- vari[depthori2 >= top]
#    }
#    mdepth <- c(top, (depth[-1] + depth[-length(depth)])/2, bot)
#    thediff <- diff(mdepth)
#    sum(thediff * vari)/sum(thediff)
#  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
