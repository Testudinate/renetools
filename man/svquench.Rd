\name{svquench}
\alias{svquench}

\title{Function to correct quenching effects}

\description{
  The functions corrects the values given by \code{Fluo} due to static
  quenching effects. Stern-Volmer-Constant needs to be given by
  \code{Kvs}. Alternativly built in constants for several
  phytoplankton species can be used.
}
\usage{
svquench(Fluo, Kvs)
}

\arguments{
  \item{Fluo}{numerical or vector of values to be corrected}

  \item{Kvs}{numerical Stern-Volmer-Constant or one of the predifined species: 
    "Ast_stag", "Aul_stag", "Fra_stag", "Ast_exp", "Aul_exp",
    "Fra_exp", which stand for Asterionella formosa, Aulacoseira
    subarctica and Fragilaria crotonensis taken from stationary or
    exponential growth phase.
  }
}

\references{
  Otto Stern, Max Volmer: Über die Abklingungszeit der
    Fluoreszenz. Physikalische Zeitschrift, 20, 183-188, (1919)

  \url{http://de.wikipedia.org/wiki/Stern-Volmer-Gleichung}
}

\author{René Sachse}

\seealso{See Also \code{\link{fddtxt2tab}}}
\examples{
svquench(200, 0.002)
svquench(c(10,100,200), "Aul_stag")

## The function is currently defined as
#function (Fluo, Kvs) 
#{
#    if (is.character(Kvs)) {
#        Kvs <- switch(Kvs, Ast_stag = 0.002175253, Aul_stag = 0.003180959, 
#            Fra_stag = 0.002693194, Ast_exp = 4, Aul_exp = 5, 
#            Fra_exp = 6)
#    }
#    F0 <- Fluo/(1 - Kvs * Fluo)
#    F0
#  }
#}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{quench}
\keyword{quenching}
