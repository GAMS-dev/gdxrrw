\name{gdxInfo}
\alias{gdxInfo}
\title{Display or Return Information from a GDX File}
\description{
Display all the information contained in a GDX file, along with some
version & debugging info.  The display format is copied from gdxdump.

Alternately, return information about the symbols contained in a GDX
file in list or a more extensive data frame format.
}
\usage{
  gdxInfo(gdxName = NULL, dump=TRUE, returnList=FALSE, returnDF=FALSE)
}
\arguments{
  \item{gdxName}{the name of the GDX file.  If this argument is
  omitted, we display only the version info for the GDX library used}
  \item{dump}{if TRUE, dump GDX contents to the console}
  \item{returnList}{if TRUE, return brief information about the
  symbols in the GDX file as a list of lists}
  \item{returnDF}{if TRUE, return extended information about the
  symbols in the GDX file as a list of data frames}
}
\author{Original coding by Rishabh Jain.  Adopted and packaged by
Steve Dirkse.  Maintainer: \email{R@gams.com}}
\note{
  A common problem is failure to load the external GDX libraries that
  are required to interface with GDX data.  Use \code{\link{igdx}} to
  troubleshoot and solve this problem.
}

\seealso{
 \code{\link{igdx}}
}
\examples{
  gdxInfo();
  \dontrun{
    gdxInfo("trnsport.gdx");
    gdxInfo("trnsport.gdx", dump=FALSE, returnDF=TRUE);
  }
}
\keyword{ data }
\keyword{ optimize }
\keyword{ interface }
