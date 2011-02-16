## Suppose a package needs to call a shared library named 'fooEXT',
## where 'EXT' is the system-specific extension.  Then you should use
.First.lib <- function(lib, pkg)
{
  library.dynam("gdxrrw", pkg, lib)
  library("gdxrrw")
}	
## FOR MORE INFORMATION please refer http://pbil.univ-lyon1.fr/library/base/html/library.html
## if you want to mask as little as possible, use
##library("testRPackage", pos = "package:base")
## End(Not run)


##if(is.loaded("gdxrrw.so"))
##	{
##		dyn.unload("gdxrrw.so")
##	}


##dyn.load("gdxrrw.so")

rgdx <- function(gdxFile, ...)
{
  .External("rgdx", gdxFile, ..., PACKAGE="gdxrrw")
}

wgdx <- function(gdxFile, ...)
{
  .External("wgdx", gdxFile, ..., PACKAGE="gdxrrw")
}

gams <- function(gms, ...)
{
  .External("gams", gms, ..., PACKAGE="gdxrrw")
}

gdxInfo <- function(gdxName = NULL)
{
  .External("gdxInfo", gdxName, PACKAGE="gdxrrw");
}

igdx <- function(...)
{
  .External("igdx", ..., PACKAGE="gdxrrw");
}
