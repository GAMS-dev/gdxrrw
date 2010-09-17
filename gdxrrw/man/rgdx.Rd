\name{rgdx}
\alias{rgdx}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read GDX
}
\description{
This function is to read symbol information from GDX file and store it in
R list format. It take up to two input arguments. First argument will be the
GDX file name in String format and second optional argument will be the 
list input to customize output list. 
}
\usage{
rgdx(gdxFile, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{gdxFile}{
Name of GDX file from which data need to be read.
}
  \item{\dots}{
List argument.
}
}
\details{
This is some extra detail about rgdx. Please read document. 
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Rishabh Jain
}
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
function (gdxFile, ...) 
{
    .External("rgdx", gdxFile, ...)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
