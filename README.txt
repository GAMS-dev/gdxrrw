This directory (gdxrrw) contains the files related to the R package
gdxrrw.  Or maybe we'll call the package gams - that remains to be
seen.

The subdirectory gdxrrw contains the files that will be used to create
the R package.  For instructions on the steps needed to create the
package, look in the docsInternal subdirectory.  Or maybe they will go
here - we'll have to see what they are first.

Quick steps for rebuilding & testing rgdx:

% \rm -r *.o *.so
% R CMD SHLIB gdxrrw.c gdxcc.c gclgms.c
% R
> dyn.load("gdxrrw.so")
> rgdx <- function(gdxFile, ...) { .External('rgdx',gdxFile, ...) }
> lst <- list(name='j',compress='true');
> rgdx('trnsport',lst)
