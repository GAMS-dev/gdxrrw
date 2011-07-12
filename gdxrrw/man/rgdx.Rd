\name{rgdx}
\alias{rgdx}
\alias{rgdx.param}
\alias{rgdx.set}
\title{Read data from GDX into R}
\description{
  Read one data item (also called a symbol) from GDX into R, returning
  it as a list.  Note that GDX files contain multiple symbols (e.g. sets,
  parameters).  Each symbol is read with a separate call.
}
\usage{
  rgdx(gdxName, requestList = NULL)  # generic form
  rgdx.param(gdxName, parName)  # return a parameter in a data frame
  rgdx.set(gdxName, setName)  # return a set in a data frame
}
\arguments{
  \item{gdxName}{the name of the GDX file to read}
  \item{requestList}{the name of the symbol to read, and (optionally)
  information about how much information to return and in what
  format.  This argument must be a list.  If omitted, the universe of
  UELs contained in the GDX file is returned}
  \item{parName}{the name of the parameter to read}
  \item{setName}{the name of the set to read}
}
\details{}
\value{
  By default, the return value is a list with components describing
  the data item or symbol returned.

  \item{name}{symbol name}
  \item{type}{symbol's data type: \code{set}, \code{parameter},
  \code{variable} or \code{equation}}
  \item{dim}{symbol dimension}
  \item{val}{array containing the symbol data}
  \item{form}{form of the data in \code{val}, i.e. \code{full} or \code{sparse}}
  \item{uels}{vector of UEL lists, one list per symbol dimension}


  The functions \code{rgdx.param} and \code{rgdx.set} are
  special-purpose wrappers that read parameters and sets,
  respectively, and return them as data frames.

  The output format can be adjusted via a global option.  Documentation
  for this is in progress.
}
\author{Original coding by Rishabh Jain.  Adopted and packaged by
Steve Dirkse.  Maintainer: \email{R@gams.com}}
\note{
  A common problem is failure to load the external GDX libraries that
  are required to interface with GDX data.  Use \code{\link{igdx}} to
  troubleshoot and solve this problem.
}
\seealso{
 \code{\link{igdx}}, \code{\link{wgdx}}, \code{\link{gdxInfo}}
}
\examples{
  # run R-script trnsport.r from the data subdirectory of the gdxrrw package
  # to load up some data for writing to a GDX file
  data(trnsport)

  wgdx('rgdx1',sf,si,sj,sa,sb,sd)
  ou = rgdx('rgdx1')
  req <- list(name='f')
  of = rgdx('rgdx1',req)
  req <- list(name='i')
  oi = rgdx('rgdx1',req)
  req <- list(name='j')
  oj = rgdx('rgdx1',req)
  req <- list(name='a')
  oa = rgdx('rgdx1',req)
  req <- list(name='b')
  ob = rgdx('rgdx1',req)
  req <- list(name='d')
  od = rgdx('rgdx1',req)

  \dontrun{
    # complete tests and examples can be run in the
    # inst/extdata directory of the gdxrrw package
    # check .libPaths for a hint on where packages are installed
    setwd("C:/R/2.12/gdxrrw/inst/extdata")
    source("tAll.R")
  }
}
\keyword{ data }
\keyword{ optimize }
\keyword{ interface }
