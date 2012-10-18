\name{gams}
\alias{gams}
\title{Run a GAMS model from R}
\description{
  Run a GAMS model from R.
}
\usage{
  gams(gmsAndArgs)
}
\arguments{
  \item{gmsAndArgs}{Name of \code{.gms} file to run, with possible args}
}
\author{Original coding by Rishabh Jain.  Adopted and packaged by
Steve Dirkse.  Maintainer: \email{R@gams.com}}
\value{Return from executing \code{gams gmsAndArgs}}
\note{
  A common problem is failure to find the GAMS system directory.  Use
  \code{\link{igdx}} to troubleshoot and solve this problem.
}
\seealso{
  \code{\link{igdx}}, \code{\link{rgdx}}, \code{\link{wgdx}}
}
\examples{
  \dontrun{
    gams("trnsport.gms")
    gams("myModel.gms lp=BDMLP --JOB_ID=case00")
  }
}
\keyword{ data }
\keyword{ optimize }
\keyword{ interface }
