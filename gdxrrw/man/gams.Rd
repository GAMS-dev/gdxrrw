\name{gams}
\alias{gams}
\title{Run a GAMS model from R}
\description{
  Run a GAMS model from R.
}
\usage{
  gams(gms, ...)
}
\arguments{
  \item{gms}{Name of \code{.gms} file to run}
  \item{\dots}{Data inputs for the GAMS model}
}
\author{Original coding by Rishabh Jain.  Adopted and packaged by
Steve Dirkse.  Maintainer: \email{R@gams.com}}
\value{
  Data outputs from the GAMS model.  This usage is deprecated: the
  recommended procedure is to write GDX files from the GAMS models and
  use the \code{rgdx} family of calls to bring the data into R.
}
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
  }
}
\keyword{ data }
\keyword{ optimize }
\keyword{ interface }
