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
\details{
  To query but not modify the current GDX library binding, leave the
  \code{gamsSysDir} argument NULL.  To clear, reload, and then query the
  GDX library binding, pass a string argument to \code{gamsSysDir}.

  If \code{gamsSysDir} is a non-empty string containing a valid
  directory, it will be tried first.  If the GDX libraries cannot be
  loaded from this location, the next load attempt will make use of the
  environment variable \code{R_GAMS_SYSDIR}, provided it is set to a
  non-empty string.  If we are still not successful, as a last resort we
  try to load the GDX libraries using the system-specific library search
  mechanism (e.g. the PATH on Windows or LD_LIBRARY_PATH on Linux).
}
\value{
  By default (when \code{returnStr} is FALSE), the return value is TRUE
  if the external GDX libraries are loaded, FALSE o/w.  If
  \code{returnStr} is TRUE, the return value is the path to the directory
  containing the external GDX libraries if these libraries were
  successfully loaded, an empty string otherwise.
}
\author{Steve Dirkse.  Maintainer: \email{R@gams.com}}
\note{
  The directory containing the external GDX libraries is also where we
  look for the gams executable, so calling \code{igdx} prior to
  calling \code{\link{gams}} ensures that the gams executable can be
  located.  Consider saving a .First function like the following to your
  R workspace:
   \code{.First <- function() { library(gdxrrw) ; igdx("/your/GAMS/sysdir")}}
  or set the environment variable \code{R_GAMS_SYSDIR}.
}

\seealso{
  \code{\link{biking}}
}
\examples{
  \dontrun{
    igdx("C:\\Program Files\\gams23.6");
    igdx("/usr/gams/23.6.3");
  }
  igdx();
}
\keyword{ datasets }
