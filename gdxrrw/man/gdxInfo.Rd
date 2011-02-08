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
  omitted, what happens?}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{Rishabh Jain}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
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
