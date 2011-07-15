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

rgdx.param <- function(gdxName, symName, names=NULL)
{
  sym <- rgdx(gdxName, list(name=symName))
  if (sym$type != "parameter") {
    stop ("Expected to read a parameter: symbol ", symName, " is a ", sym$type)
  }
  symDim <- sym$dim
  if (symDim < 1) {
    stop ("Symbol ", symName, " is a scalar: data frame output not possible")
  }

  fnames <- list()
  if (is.null(names)) {
    if (1 == symDim) {
      fnames <- list("i","val")
    } else if (2 == symDim) {
      fnames <- list("i","j","val")
    } else if (3 == symDim) {
      fnames <- list("i","j","k","val")
    } else {
      for (d in c(1:symDim)) {
        fnames[[d]] <- paste("i",d,sep="")
      }
      fnames[[symDim+1]] <- "val"
    }
  } else {
    # process the user-provided names
    if (is.list(names)) {
      namlen <- length(names)
      d2 <- 1
      for (d in c(1:symDim)) {
        fnames[[d]] <- as.character(names[[d2]])
        d2 <- d2+1
        if (d2 > namlen) d2 <- 1
      }
      # consider 2 cases: names provided just for the index cols,
      # or for the data column too
      if (namlen <= symDim) {
        fnames[[symDim+1]] <- "val"
      }
      else {
        fnames[[symDim+1]] <- as.character(names[[d2]])
      }
    } else {
      for (d in c(1:symDim)) {
        fnames[[d]] <- paste(as.character(names),d,sep=".")
      }
      fnames[[symDim+1]] <- "val"
    }
    fnames <- make.names(fnames,unique=TRUE)
  }

  dflist <- list()
  for (d in c(1:symDim)) {
    nUels <- length(sym$uels[[d]])
    dflist[[fnames[[d]]]] <- factor(sym$val[,d], seq(to=nUels), labels=sym$uels[[d]])
  }
  dflist[[fnames[[symDim+1]]]] <- sym$val[,symDim+1]
  symDF <- data.frame(dflist)
  return(symDF)
} # rgdx.param

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
