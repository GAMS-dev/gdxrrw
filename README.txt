This directory (gdxrrw) contains the files related to the R package
gdxrrw.  Here's a quick overview:

- gdxrrw/
  -- docsInternal - docs for the developers, not the users.
  -- gdxrrw/ - package subdirectory, with contents and organization to
     work with the R package management system
  -- inspect/ - C source for shared library to experiment with.  Uses
     same convention (.External) as gdxrrw routines.
  -- knitr/ - some knitr examples and sandbox-type stuff
  -- reshape/ - initial work on wgdx.reshape
  -- sandbox/ - all sorts of stuff worth saving but not part of the
     package distribution
  -- README.txt (this file)

For ideas on the steps needed to create the package see the file
docsInternal/dirkseLog.txt.  This file is more a log of what has
worked than an instruction manual.  There are already plenty of
instruction manuals on R package management,
e.g. docsInternal/CreatingPackages.pdf so why write another one?

