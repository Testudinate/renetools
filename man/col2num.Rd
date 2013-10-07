\name{col2num}
\alias{col2num}
\title{Function to convert column to numeric}
\description{
Converts given columns of a data.frame to numeric.
}
\usage{
col2num(dat, cols = c(2, 3, 5, 12, 13))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{data.frame}
  \item{cols}{number of columns to be converted to numeric}
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
\seealso{See Also as \code{\link{col2char}}, \code{\link{col2date}}}
\examples{

dat <- data.frame(x = c("5","12","3"), y=3:1)
str(dat) #1st column is of type factor
dat <- col2num(dat, 1)
str(dat) #1st column is of type numeric

## The function is currently defined as
#function(dat, cols=c(2,3,5,12,13)){
#  for(i in cols){
#    dat[,i] <- as.character(dat[,i])
#    dat[,i] <- as.numeric(dat[,i])
#  }
#  dat
#}
}
