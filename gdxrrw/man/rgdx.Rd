\name{rgdx}
\alias{rgdx}
\alias{rgdx.param}
\alias{rgdx.scalar}
\alias{rgdx.set}
\title{Read data from GDX into R}
\description{
  Read one data item (also called a symbol) from GDX into R, returning
  it as a list.  Note that GDX files contain multiple symbols (e.g. sets,
  parameters).  Each symbol is read with a separate call.
}
\usage{
  # generic form - return a symbol as a list
  rgdx(gdxName, requestList = NULL, squeeze = TRUE, useDomInfo = TRUE)

  # return a set or parameter in a data frame
  rgdx.set(gdxName, symName, names=NULL, compress=FALSE, ts=FALSE, useDomInfo = TRUE)
  rgdx.param(gdxName, symName, names=NULL, compress=FALSE, ts=FALSE, squeeze=TRUE, useDomInfo = TRUE)

  # return a scalar
  rgdx.scalar(gdxName, symName, ts=FALSE)
}
\arguments{
  \item{gdxName}{the name of the GDX file to read}
  \item{requestList}{the name of the symbol to read, and (optionally)
  information about how much information to return and in what
  format.  This argument must be a named list.  If omitted, the
  universe of UELs contained in the GDX file is returned}
  \item{squeeze}{if TRUE/nonzero, squeeze out any zero or EPS stored in the GDX container}
  \item{useDomInfo}{if TRUE, the default filter will be the domain info
  in the GDX.  If no domain info is available, or if
  \code{useDomInfo=FALSE}, the default filter will be the GDX universe}
  \item{symName}{the name of the GDX symbol to read}
  \item{names}{the column names to use in the data frame returned}
  \item{compress}{if TRUE, compress the factors in the data frame so they only include required levels.  For the default compress=FALSE, each factor includes levels for the entire universe of UELs in the GDX file}
  \item{ts}{if TRUE, include the \code{.ts} field (i.e. the explanatory text) for the symbol in the return}
}
\details{
  The \code{requestList} argument to \code{rgdx} is essentially a list
  of arguments specifying what symbol to read and how to read it.  Valid
  list members are:
  \describe{
    \item{name}{name of symbol to read from GDX}
    \item{form}{specify representation to use on return: \dQuote{sparse}
      (default) or \dQuote{full}}
    \item{uels}{UEL filter to use when reading}
    \item{field}{specify field to read for equations and variables}
    \item{te}{if true, return the associated text (i.e. the \code{.te}
      field) for each set element in the return}
    \item{ts}{if true, include the explanatory text (i.e. the \code{.ts}
      field) for the symbol in the return}
    \item{compress}{if true, compress UEL lists of return value by removing
      unused elements in each index position}
    \item{dim}{expected dimension of symbol to be read}
  }
}
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

  The function \code{rgdx.scalar} returns a scalar (i.e. a
  0-dimensional parameter) as a double.

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

  wgdx("rgdx1",sf,si,sj,sa,sb,sd)
  ou = rgdx("rgdx1")
  req <- list(name="f")
  of = rgdx("rgdx1",req)
  req <- list(name="i")
  oi = rgdx("rgdx1",req)
  req <- list(name="j")
  oj = rgdx("rgdx1",req)
  req <- list(name="a")
  oa = rgdx("rgdx1",req)
  req <- list(name="b")
  ob = rgdx("rgdx1",req)
  req <- list(name="d")
  od = rgdx("rgdx1",req)

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
