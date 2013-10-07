\name{joincolumns}
\alias{joincolumns}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function for comparing two tables by means of a common identifier
and to create a new column for on of the frames with data from the other table
}
\description{
  This function compares which rows of an ID of a data.frame matches the ID of 
  another data.frame and adds a new column with data to the first table.
}
\usage{
joincolumns(dat1, dat2, common1, common2, what1, what2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat1}{a data.frame to which data should be added}
  \item{dat2}{a data.frame from which data is to be extracted}
  \item{common1}{name of the column for the ID in dat1}
  \item{common2}{name of the column for the ID in dat2}
  \item{what1}{name of the new column for dat1}
  \item{what2}{name of the column of dat2 providing the data of interest}
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
\examples{
dat1 <- data.frame(ID=seq(1,12,2), V1=rnorm(6))
dat2 <- data.frame(id=1:20, V2=21:40)
dat1 <- joincolumns(dat1, dat2, "ID", "id", "dat2", "V2")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{join tables}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
