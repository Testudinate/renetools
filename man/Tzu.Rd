\name{Tzu}
\alias{Tzu}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to estimate water temperature of streams by using current and
previous measurements of air temperatures.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Tzu(b1, b2, b3, b4, b5, b6, m, doy, tl)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{b1}{model parameter
}
  \item{b2}{model parameter
%%     ~~Describe \code{b2} here~~
}
  \item{b3}{model parameter
%%     ~~Describe \code{b3} here~~
}
  \item{b4}{model parameter
%%     ~~Describe \code{b4} here~~
}
  \item{b5}{model parameter
%%     ~~Describe \code{b5} here~~
}
  \item{b6}{model parameter
%%     ~~Describe \code{b6} here~~
}
  \item{m}{ model parameter defining the number of previous days which have
influence on stream temperature
%%     ~~Describe \code{m} here~~
}
  \item{doy}{vector of days of the year 
%%     ~~Describe \code{doy} here~~
}
  \item{tl}{vector of air temperatures at the given days
%%     ~~Describe \code{tl} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns estimated water temperatures, starting from day m until last given day.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{

Horn, H., Horn, W., Paul, L., Uhlmann, D. & Roeske, I. (2006). Drei
Jahrzehnte kontinuierliche Untersuchungen an der Talsperre
Saidenbach. Fakten, Zusammenhaenge, Trends. Abschlussbericht zum
Projekt "Langzeitstabilitaet der biologischen Struktur von Talsperren-
Oekosystemen" der Arbeitgruppe "Limnologie von Talsperren" der
Saechsischen Akademie der Wissenschaften zu Leipzig.

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
data(tairdat)
p      <- c(5.81, 3.67, -0.0657, 0.365, 0.166, 252)
Twater <- Tzu(p[1], p[2], p[3], p[4], p[5], p[6], 5, tairdat$doy, tairdat$Tluft)
plot(tairdat$doy, tairdat$Tluft, pch=16, ylab="T in degree C", xlab="doy", las=1)
lines(tairdat$doy, tairdat$TWasser, col="lightblue")
lines(tairdat$doy, Twater, col="blue", lwd=2)
legend(
"bottomleft",
  c("T Air", "T Water measurement", "T Water model"),
  pch=c(16, NA, NA),
  lty=c(NA, 1, 1),
  col=c("black", "lightblue", "blue"),
  bty="n"
)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
