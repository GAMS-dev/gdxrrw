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


rgdx <- function(gdxName, requestList = NULL)
{
  .External("rgdx", gdxName, requestList, PACKAGE="gdxrrw");
}

wgdx <- function(gdxName, ...)
{
  .External("wgdx", gdxName, ..., PACKAGE="gdxrrw")
}

gams <- function(gms, ...)
{
  .External("gams", gms, ..., PACKAGE="gdxrrw")
}

gdxInfo <- function(gdxName = NULL)
{
  .External("gdxInfo", gdxName, PACKAGE="gdxrrw");
}

igdx <- function(gamsSysDir = NULL)
{
  .External("igdx", gamsSysDir, PACKAGE="gdxrrw");
}

rgdx.param <- function(gdxName, parName)
{
  request <- list(name=parName)
  readpar <- rgdx(gdxName, request)
  if (readpar$type != "parameter") {
    stop ("Expected to read a parameter: symbol ", parName, " is a ", readpar$type)
  }
  dimpar <- readpar$dim
  if (dimpar < 1) {
    stop ("Symbol ", parName, " is a scalar: data frame output not possible")
  }
  lengthval <- length(readpar$val[,1])
  X1 <- matrix(NA,nrow=lengthval,ncol=dimpar)
  readpardf <- data.frame(X1)
  for (i in c(1:lengthval)) {
    readpardf[i,dimpar+1] <- readpar$val[i,dimpar+1]
    for (j in c(1:dimpar)) {
      readpardf[i,j] <- readpar$uels[[j]][readpar$val[i,j]]
    }
  }
  return(readpardf)
} # rgdx.param

rgdx.pp <- function(gdxName, symName)
{
  sym <- rgdx(gdxName, list(name=symName))
  if (sym$type != "parameter") {
    stop ("Expected to read a parameter: symbol ", symName, " is a ", sym$type)
  }
  symDim <- sym$dim
  if (symDim < 1) {
    stop ("Symbol ", symName, " is a scalar: data frame output not possible")
  }
#  nrows <- dim(sym$val)[1]
  flist <- list()
  for (d in c(1:symDim)) {
    fname <- paste("factor",d,sep="-")
    nUels <- length(sym$uels[[d]])
    flist[[fname]] <- factor(sym$val[,d], seq(to=nUels), labels=sym$uels[[d]])
  }
  symDF <- data.frame(flist,sym$val[,symDim+1])
  return(symDF)
} # rgdx.pp

rgdx.scalar <- function(gdxName, symName)
{
  request <- list(name=symName)
  readsym <- rgdx(gdxName, request)
  if (readsym$type != "parameter") {
    stop ("Expected to read a scalar: symbol ", symName, " is a ", readsym$type)
  }
  dimsym <- readsym$dim
  if (dimsym > 0) {
    stop ("Parameter ", symName, " has dimension ", dimsym, ": scalar output not possible")
  }
  c <- readsym$val[1,1]
  return(c)
} # rgdx.scalar

rgdx.set <- function(gdxName, setName)
{
  request <- list(name=setName)
  readset <- rgdx(gdxName, request)
  if (readset$type != "set") {
    stop ("Expected to read a set: symbol ", setName, " is a ", readset$type)
  }
  dimset <- readset$dim
  lengthval <- length(readset$val[,1])
  X1 <- matrix(NA,nrow=lengthval,ncol=dimset)
  readsetdf <- data.frame(X1)
  for (i in c(1:lengthval)) {
    for (j in c(1:dimset)) {
      readsetdf[i,j] <- readset$uels[[j]][readset$val[i,j]]
    }
  }
  return(readsetdf)
} # rgdx.set
