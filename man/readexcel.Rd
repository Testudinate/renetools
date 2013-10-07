\name{readexcel}
\alias{readexcel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function to load MS-Excel sheets}
\description{
  Loads MS-Excel sheets.
}
\usage{
  readexcel(file, table)
}


\arguments{
  \item{file}{path and filename which shall be opened}
  \item{table}{name of the table within the Excel sheet}
}
\value{
 a data frame containig the tabular data.
}

\author{\enc{René}{Rene} Sachse}

\seealso{See Also \code{\link{odbcConnectExcel}}}
\examples{
  \dontrun{tiny <- readexcel("2010-11-09-PETLINGvsTINY", "tiny")}
}
\keyword{error bars}
