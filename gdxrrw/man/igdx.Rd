\name{igdx}
\alias{igdx}
\title{Initialize the External GDX Libraries}
\description{
  Initialize (i.e. load) the external GDX libraries that are required
  to interface with GDX data, and report on the status of the
  initialization.
}
\usage{igdx(gamsSysDir=NULL, silent=FALSE)}
\arguments{
  \item{gamsSysDir}{a directory containing the external GDX libraries,
  typically the GAMS system directory.  If this argument is omitted,
  no loading is done but we still report on the initialization
  status.}
  \item{silent}{controls logging of results}
}
\author{Steve Dirkse.  Maintainer: \email{R@gams.com}}
\note{
  The directory containing the external GDX libraries is also where we
  look for the gams executable, so calling \code{igdx} prior to
  calling \code{\link{gams}} ensures that the gams executable can be
  located.  Consider saving a .First function like the following to your
  R workspace:
   \code{.First <- function() { library(gdxrrw) ; igdx("/your/GAMS/sysdir")}}
}

\seealso{
  \code{\link{rgdx}}, \code{\link{wgdx}}, \code{\link{gams}},
  \code{\link{gdxInfo}} 
}
\examples{
  \dontrun{
    igdx("C:\\Program Files\\gams23.6");
    igdx("/usr/gams/23.6.3");
  }
  igdx();
}
\keyword{ data }
\keyword{ optimize }
\keyword{ interface }
