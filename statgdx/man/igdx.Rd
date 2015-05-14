\name{igdx}
\alias{igdx}
\title{Initialize the External GDX Libraries}
\description{
  Initialize (i.e. load) the external GDX libraries that are required
  to interface with GDX data, and report on the status of the
  initialization.
}
\usage{igdx(gamsSysDir=NULL, silent=FALSE, returnStr=FALSE)}
\arguments{
  \item{gamsSysDir}{a directory containing the external GDX libraries,
  typically the GAMS system directory.  If this argument is omitted,
  no loading is done but we still report on the initialization
  status.}
  \item{silent}{controls logging of results}
  \item{returnStr}{controls what to return.  If FALSE, return TRUE if
    the external GDX libraries are loaded, FALSE o/w.  If TRUE, return
    the path to the external GDX libraries if loaded, an empty string
    otherwise.}
}
\author{Steve Dirkse.  Maintainer: \email{R@gams.com}}
\seealso{
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
