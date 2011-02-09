\name{gdxInfo}
\alias{gdxInfo}
\title{gdxInfo}
\description{
Display all the information contained in a GDX file, along with some
version & debugging info.  The display format is copied from gdxdump.
}
\usage{gdxInfo(gdxFile)
}
\arguments{
  \item{gdxFile}{the name of the GDX file.  If this argument is
  omitted, we display only the version info for the GDX library used}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{Original coding by Rishabh Jain.  Adopted and packaged by
Steve Dirkse.  Maintainer: <R@gams.com>}
\note{
  A common problem is failure to load the external GDX libraries that
  are required to interface with GDX data.  Use \code{\link{igdx}} to
  troubleshoot and solve this problem.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{igdx}}, \code{\link{rgdx}}, \code{\link{wgdx}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (gdxFile) 
{
    .External("gdxInfo", gdxFile)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ data }
\keyword{ optimize }
\keyword{ interface }
