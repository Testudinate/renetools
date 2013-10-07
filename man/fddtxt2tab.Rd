\name{fddtxt2tab}
\alias{fddtxt2tab}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function to import data from TXT-Files of the Fluospectrometer}
\description{
Imports data directly from TXT-Files from the Fluospectrometer of the Institute
of Hydrobiologie, TU-Dresden
}
\usage{
fddtxt2tab(pfadr)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pfadr}{path to a folder containing TXT-Files}
}
\details{
The function processes all TXT-Files found beneath the given directory.
}
\value{
Returns a data.frame with the wavelength ("lambda") and recorded fluorescence ("F"),
filename will be given in the column "sample"
}
%\references{ ~put references to the literature/web site here ~ }
\author{René Sachse}
%\note{ ~~further notes~~ 
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{See Also \code{\link{svquench}}}

\examples{
\dontrun{fddtxt2tab("d:/Daten/Fluo/Experiment5/")}

## The function is currently defined as
#function (pfadr) 
#{
#    files <- getdocs(pfadr)
#    lf <- length(files)
#    out <- NULL
#    for (i in 1:lf) {
#        print(paste(i, files[i]))
#        dat <- readLines(myfile(pfadr, files[i]))
#        ld <- length(dat)
#        start <- grep("#DATA", dat) + 1
#        tdat <- dat[start:ld]
#        s1 <- strsplit(tdat, "\t")
#        s1 <- lapply(s1, as.numeric)
#        s1 <- s1[-length(s1)]
#        sout <- NULL
#        for (l in 1:length(s1)) {
#            s2 <- data.frame(sample = sub(".TXT", "", files[i]), 
#                lambda = s1[[l]][1], F = s1[[l]][2])
#            sout <- rbind(sout, s2)
#        }
#        out <- rbind(out, sout)
#    }
#    out
#  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Dresden}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
