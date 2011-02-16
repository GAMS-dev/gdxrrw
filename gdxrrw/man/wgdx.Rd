\name{wgdx}
\alias{wgdx}
\title{Write R data to GDX}
\description{
  Create a GDX file containing the GAMS data described in the
  lists \dots.  Note that each list describes a separate
  data element - i.e. a set or parameter - all of which are written to
  a single file.
}
\usage{wgdx(gdxName, ...)}
\arguments{
  \item{gdxName}{the name of the GDX file to write}
  \item{\dots}{zero or more lists describing the sets and
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
  data(trnsport);
  wgdx('wgdx1.gdx',sf,si,sj,sa,sb,sd);
}
\keyword{ data }
\keyword{ optimize }
\keyword{ interface }
