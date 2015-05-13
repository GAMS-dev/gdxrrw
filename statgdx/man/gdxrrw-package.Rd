\name{gdxrrw-package}
\alias{gdxrrw-package}
\alias{gdxrrw}
\docType{package}
\title{Exchanging data between GAMS and R}
\description{
This package implements an interface between GAMS and R.  It includes
functions to transfer data between GDX (the GAMS data format) and R, a
function to call GAMS from within R, query functions for the meta-data
in GDX, and other related utilities.
}
\details{
This package contains five classes of functions:
\itemize{
  \item{igdx(...) gives information on the linkage between this
  package and the GDX library.}
  \item{rgdx("gdxfile", ...) and related functions read from GDX}
  \item{wgdx("gdxfile", ...) and related functions write to GDX}
  \item{gams("gmsfile and args", ...) runs gams with the arguments
  provided}
  \item{gdxInfo("gdxfile", ...) dumps GDX content or returns GDX
  metadata (list of symbols, etc.)} 
}
}
\author{Original coding by Rishabh Jain.  Adopted, packaged, and extended by
Steve Dirkse.  Maintainer: \email{R@gams.com}}
\keyword{ package }
\keyword{ data }
\keyword{ optimize }
\keyword{ interface }
% \examples{ }
