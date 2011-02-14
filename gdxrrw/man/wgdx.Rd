\name{wgdx}
\alias{wgdx}
\title{Write R data to GDX}
\description{
  Create a GDX file containing the GAMS data described in the
  structures \dots.  Note that each structure describes a separate
  data element - i.e. a set or parameter - all of which are written to
  a single file.
}
\usage{wgdx(gdxFileNam, ...)}
\arguments{
  \item{gdxFile}{the name of the GDX file}
  \item{\dots}{zero or more structures describing the sets and
  parameters to write}
}
\details{}
\author{Original coding by Rishabh Jain.  Adopted and packaged by
Steve Dirkse.  Maintainer: \email{R@gams.com}}
\note{
  A common problem is failure to load the external GDX libraries that
  are required to interface with GDX data.  Use \code{\link{igdx}} to
  troubleshoot and solve this problem.
}
\seealso{
 \code{\link{igdx}}, \code{\link{rgdx}}, \code{\link{gdxInfo}}
}
\examples{
  data(wgdx1);
  wgdx('wgdx1.gdx',sf,si,sj,sa,sb,sd);
}
\keyword{ data }
\keyword{ optimize }
\keyword{ interface }
