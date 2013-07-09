\name{wgdx}
\alias{wgdx}
\alias{wgdx.lst}
\title{Write R data to GDX}
\description{
  Create a GDX file containing the GAMS data described in the
  lists \dots.  Note that each list describes a separate
  data element - i.e. a set or parameter - all of which are written to
  a single file.
}
\usage{
  # generic form - each arg is a list specifying a symbol to write
  wgdx(gdxName, ..., squeeze = 'y')

  # write multiple symbols specified in list, data frame, or scalar form
  wgdx.lst(gdxName, ..., squeeze = 'y')
}
\arguments{
  \item{gdxName}{the name of the GDX file to write}
  \item{\dots}{zero or more arguments describing the symbols to write.
  For \code{wgdx}, each arg must be a list holding a symbol (e.g. as
  returned by \code{rgdx)}.  For
  \code{wgdx.lst}, each arg is also allowed to be a data frame holding a
  symbol (e.g. as returned by \code{rgdx.param} or \code{rgdx.set}), a
  scalar holding a symbol (e.g. as returned
  by \code{rgdx.scalar}), or a list containing any combination of these
  elements.}
  \item{squeeze}{if 'y'/TRUE/nonzero, squeeze out zeros: do not store
  in GDX.  If 'n'/FALSE/zero, do not squeeze out zeros: store explicit
  zeros in GDX.  If 'e', store zeros as EPS in the GDX.}
}
% \details{}
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
  wgdx("wgdx1.gdx",sf,si,sj,sa,sb,sd);

  \dontrun{
    # complete tests and examples can be run in the
    # extdata directory of the gdxrrw package
    # check .libPaths for a hint on where packages are installed
    setwd(paste(.libPaths()[1],"/gdxrrw/extdata",sep=""))
    source("tAll.R")
  }
}
\keyword{ data }
\keyword{ optimize }
\keyword{ interface }
