\name{wgdx}
\alias{wgdx}
\alias{wgdx.df}
\alias{wgdx.scalar}
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

  # write a data frame - inverse of \code{rgdx.param} and \code{rgdx.set}
  wgdx.df(gdxName, df, squeeze = 'y')

  # write a scalar - inverse of \code{rgdx.scalar}
  wgdx.scalar(gdxName, s)

  # write multiple symbols
  wgdx.lst(gdxName, ilst, squeeze = 'y')
}
\arguments{
  \item{gdxName}{the name of the GDX file to write}
  \item{\dots}{zero or more lists describing the sets and
  parameters to write}
  \item{squeeze}{if 'y'/TRUE/nonzero, squeeze out zeros: do not store
  in GDX.  If 'n'/FALSE/zero, do not squeeze out zeros: store explicit
  zeros in GDX.  If 'e', store zeros as EPS in the GDX.}
  \item{df}{data frame to write to GDX}
  \item{s}{scalar to write to GDX}
  \item{ilst}{list of inputs, each of which can be a data frame as
  returned by \code{rgdx.param} or \code{rgdx.set}, a scalar as
  returned by \code{rgdx.scalar}, or a symbol list as required for
  input to \code{wgdx}}
}
% \details{}
\author{Original coding by Rishabh Jain.  Adopted and packaged by
Steve Dirkse.  Maintainer: \email{R@gams.com}}
\note{
  A common problem is failure to load the external GDX libraries that
  are required to interface with GDX data.  Use \code{\link{igdx}} to
  troubleshoot and solve this problem.

  The routines \code{wgdx.df} and \code{wgdx.scalar} are experimental
  and perhaps temporary.  Don't use them for production work.
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
