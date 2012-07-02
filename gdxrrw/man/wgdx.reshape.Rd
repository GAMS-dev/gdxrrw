\name{wgdx.reshape}
\alias{wgdx.reshape}
\title{Reshape R data to prepare it for writing to GDX}
\description{
  Reshape the input data frame (assumed to be in "wide" form) into a "long" form
  suitable for passing to \code{wgdx.lst} or \code{wgdx.df}.
}
\usage{
  wgdx.reshape(inDF, symDim, symName=NULL, tName="time",
               gdxName=NULL, setsToo=TRUE, order=NULL,
               setNames=NULL)
}
\arguments{
  \item{inDF}{data frame in "wide" form}
  \item{symDim}{dimension of the output GDX symbol}
  \item{symName}{name of the output GDX symbol}
  \item{tName}{index set name for the new index position created by reshaping}
  \item{gdxName}{name of the GDX file to write}
  \item{setsToo}{if TRUE, extract the index sets defined by \code{inDF}}
  \item{order}{specify the selection and ordering of the index columns
    when reshaping}
  \item{setNames}{specify explanatory text for the extracted index sets}
}
\author{Original coding by Steve Dirkse.  Maintainer: \email{R@gams.com}}
\note{
  A common problem is failure to load the external GDX libraries that
  are required to interface with GDX data.  Use \code{\link{igdx}} to
  troubleshoot and solve this problem.

  There is also the issue of our dependence on the \code{reshape}
  package.  This still needs to be tightened up.

  This routine is experimental/prototype work
  and perhaps temporary.  Don't use it for production work.
}
\seealso{
 \code{\link{wgdx}}, \code{\link{igdx}}, \code{\link{rgdx}}, \code{\link{gdxInfo}}
}
\examples{
  # take a sample dataset and reshape it
  str(airquality);
  oList <- wgdx.reshape (airquality, 3, symName="airquality",
                         tName = "dataType", order=c(5,6,0))
  \dontrun{
    # send the data to GDX
    wgdx.lst("airquality", oList)
  }

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
