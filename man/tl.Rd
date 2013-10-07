\name{tl}
\alias{tl}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function to plot time lines}
\description{
Provides predifined names for months and theire corresponding day and weeknumbers
of start and end. Nice for plotting time lines.
}
\usage{
tl(jahr, lang = "english")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{jahr}{the year for which cardinal dates of the months are wanted}
  \item{lang}{language of output. Can be one of
    \code{german, english, germanfull, englishfull}. Also can be vector of 
    \code{length=12} and will be used as labels instead.}
}
%\details{
%  ~~ If necessary, more details than the description above ~~
%}
\value{
A data.frame is returned with the columns: \code{d} (doy), \code{m} (month), 
\code{w} (week). The values of \code{d} and \code{m} represent the start of the 
month. 
}
%\references{ ~put references to the literature/web site here ~ }
\author{René Sachse}
%\note{ ~~further notes~~ 
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{\code{\link{axisb}}}
\examples{
tl(2008)
plot(
  1:365, 
  20 * sin(1:365/100), 
  type="l",
  axes=FALSE, frame.plot=TRUE, 
  xlab="", ylab="T"
)
axis(2)
axisb(1,c(tl(2008)$d,365), tl(2008)$m)  


## The function is currently defined as
#function (jahr, lang = "english") 
#{
#    langavail <- c("german", "english", "germanfull", "englishfull")
#    if (length(lang) == 1) {
#        langcheck <- lang \%in\% langavail
#        if (langcheck == FALSE) {
#            warning(paste(lang, "as chosen language not available! Fall back to English abbreviations."))
#            lang <- "english"
#        }
#        monate <- switch(lang, german = c("Jan", "Feb", "Mar", 
#            "Apr", "Mai", "Jun", "Jul", "Aug", "Sep", "Okt", 
#            "Nov", "Dez"), english = c("Jan", "Feb", "Mar", "Apr", 
#            "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", 
#            "Dec"), germanfull = c("Januar", "Februar", "März", 
#            "April", "Mai", "Juni", "Juli", "August", "September", 
#            "Oktober", "November", "Dezember"), englishfull = c("January", 
#            "February", "March", "April", "May", "June", "July", 
#            "August", "September", "October", "November", "December"), 
#            )
#    }
#    else {
#        if (length(lang) == 12) {
#            monate <- lang
#        }
#        else {
#            stop("Argument lang of inproper length! Vector of names needs 12 elements.")
#        }
#    }
#    tage <- NULL
#    woche <- NULL
#    for (i in 1:12) {
#        tage <- c(tage, format(as.POSIXct(paste(jahr, "/", i, 
#            "/", "01", sep = "")), "\%j"))
#        woche <- c(woche, format(as.POSIXct(paste(jahr, "/", 
#            i, "/", "01", sep = "")), "\%W"))
#    }
#    data.frame(d = as.numeric(tage), m = as.character(monate), 
#        w = as.numeric(woche))
#  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
