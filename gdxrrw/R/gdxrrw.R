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

# todo: specify one index as a column header or crosstab
rgdx.param <- function(gdxName, symName, names=NULL, crosstab=NULL)
# rgdx.param <- function(gdxName, symName, names=NULL)
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
    if (is.vector(names)) {
      namlen <- length(names)
      d2 <- 1
      for (d in c(1:symDim)) {
        fnames[[d]] <- as.character(names[d2])
        d2 <- d2+1
        if (d2 > namlen) d2 <- 1
      }
      # consider 2 cases: names provided just for the index cols,
      # or for the data column too
      if (namlen <= symDim) {
        fnames[[symDim+1]] <- "val"
      }
      else {
        fnames[[symDim+1]] <- as.character(names[d2])
      }
    } else if (is.list(names)) {
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

  ct <- -1;
  if (! is.null(crosstab)) {
    if ((! is.vector(crosstab)) || (1 != length(crosstab))) {
      stop ("crosstab argument must be an index position or name")
    }
    if (is.numeric(crosstab)) {
      ct <- crosstab
      if ((ct - round(ct)) != 0) {
        stop ("crosstab argument ", crosstab, " is not a valid index")
      }
      if ((ct < 1) || (ct > symDim)) {
        stop ("crosstab argument ", crosstab, " is not in range")
      }
      print (paste("crosstab looks good as integer:",ct))
    } else if (is.character(crosstab)) {
      ctc <- crosstab
      for (d in c(1:symDim)) {
        if (ctc == fnames[[d]]) {
          ct <- d
          break
        }
      }
      if (ct < 0) {
        stop ("crosstab argument ", ctc, " not found in list of names: ", fnames);
      } else {
        print (paste("crosstab input",ctc,"converted to integer",ct))
      }
    } else {
      stop ("crosstab argument must be an index position or name")
    }
  }
  dflist <- list()
  for (d in c(1:symDim)) {
    nUels <- length(sym$uels[[d]])
    dflist[[fnames[[d]]]] <- factor(sym$val[,d], seq(to=nUels), labels=sym$uels[[d]])
  }
  dflist[[fnames[[symDim+1]]]] <- sym$val[,symDim+1]
  symDF <- data.frame(dflist)
  if (ct > 0) {
    tv <- fnames[[ct]]
    idv <- c()
    d2 <- 1
    for (d in c(1:symDim)) {
      if (d != ct) {
        idv <- c(idv,fnames[[d]])
        d2 <- d2 + 1
      }
    }
    symCT <- reshape(symDF, v.names=fnames[[symDim+1]], idvar=idv, timevar=tv,
    	             direction="wide")
    names(symCT) <- c(idv,levels(symDF[[ct]]))
    rm(symDF)
    rm(symCT)
    stop("crosstab is not working yet: reshape only good for fully dense data");
    # return(symCT)
  } else {
    return(symDF)
  }
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

# todo: specify one index as a column header or crosstab
# rgdx.set <- function(gdxName, symName, names=NULL, crosstab=NULL)
rgdx.set <- function(gdxName, symName, names=NULL)
{
  sym <- rgdx(gdxName, list(name=symName))
  if (sym$type != "set") {
    stop ("Expected to read a set: symbol ", symName, " is a ", sym$type)
  }
  symDim <- sym$dim

  fnames <- list()
  if (is.null(names)) {
    if (1 == symDim) {
      fnames <- list("i")
    } else if (2 == symDim) {
      fnames <- list("i","j")
    } else if (3 == symDim) {
      fnames <- list("i","j","k")
    } else {
      for (d in c(1:symDim)) {
        fnames[[d]] <- paste("i",d,sep="")
      }
    }
  } else {
    # process the user-provided names
    if (is.vector(names)) {
      namlen <- length(names)
      d2 <- 1
      for (d in c(1:symDim)) {
        fnames[[d]] <- as.character(names[d2])
        d2 <- d2+1
        if (d2 > namlen) d2 <- 1
      }
    } else if (is.list(names)) {
      namlen <- length(names)
      d2 <- 1
      for (d in c(1:symDim)) {
        fnames[[d]] <- as.character(names[[d2]])
        d2 <- d2+1
        if (d2 > namlen) d2 <- 1
      }
    } else {
      for (d in c(1:symDim)) {
        fnames[[d]] <- paste(as.character(names),d,sep=".")
      }
    }
    fnames <- make.names(fnames,unique=TRUE)
  }

  dflist <- list()
  for (d in c(1:symDim)) {
    nUels <- length(sym$uels[[d]])
    dflist[[fnames[[d]]]] <- factor(sym$val[,d], seq(to=nUels), labels=sym$uels[[d]])
  }
  symDF <- data.frame(dflist)
  return(symDF)
} # rgdx.set
