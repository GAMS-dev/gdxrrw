\name{gdxInfo}
\alias{gdxInfo}
\title{Display Information in a GDX File}
\description{
Display all the information contained in a GDX file, along with some
version & debugging info.  The display format is copied from gdxdump.
}
\usage{
  gdxInfo(gdxName = NULL)
}
\arguments{
  \item{gdxName}{the name of the GDX file.  If this argument is
  omitted, we display only the version info for the GDX library used}
}
\author{Original coding by Rishabh Jain.  Adopted and packaged by
Steve Dirkse.  Maintainer: \email{R@gams.com}}
\note{
  A common problem is failure to load the external GDX libraries that
  are required to interface with GDX data.  Use \code{\link{igdx}} to
  troubleshoot and solve this problem.
}

\seealso{
 \code{\link{igdx}}, \code{\link{rgdx}}, \code{\link{wgdx}}
}
\examples{
  gdxInfo();
  \dontrun{gdxInfo("trnsport.gdx");}
}
\keyword{ data }
\keyword{ optimize }
\keyword{ interface }
