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
  .External("rgdx", gdxName, requestList, PACKAGE="gdxrrw")
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
  .External("gdxInfo", gdxName, PACKAGE="gdxrrw")
}

igdx <- function(gamsSysDir = NULL)
{
  .External("igdx", gamsSysDir, PACKAGE="gdxrrw")
}

rgdx.param <- function(gdxName, symName, names=NULL, compress=FALSE, ts=FALSE)
{
  sym <- rgdx(gdxName, list(name=symName,compress=compress,ts=ts))
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
      fnames <- list("i","value")
    } else if (2 == symDim) {
      fnames <- list("i","j","value")
    } else if (3 == symDim) {
      fnames <- list("i","j","k","value")
    } else {
      for (d in c(1:symDim)) {
        fnames[[d]] <- paste("i",d,sep="")
      }
      fnames[[symDim+1]] <- "value"
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
        fnames[[symDim+1]] <- "value"
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
        fnames[[symDim+1]] <- "value"
      }
      else {
        fnames[[symDim+1]] <- as.character(names[[d2]])
      }
    } else {
      for (d in c(1:symDim)) {
        fnames[[d]] <- paste(as.character(names),d,sep=".")
      }
      fnames[[symDim+1]] <- "value"
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
  attr(symDF,"symName") <- sym$name
  if (ts) {
    attr(symDF,"ts") <- sym$ts
  }
  return(symDF)
} # rgdx.param

rgdx.scalar <- function(gdxName, symName, ts=FALSE)
{
  request <- list(name=symName,ts=ts)
  readsym <- rgdx(gdxName, request)
  if (readsym$type != "parameter") {
    stop ("Expected to read a scalar: symbol ", symName, " is a ", readsym$type)
  }
  dimsym <- readsym$dim
  if (dimsym > 0) {
    stop ("Parameter ", symName, " has dimension ", dimsym, ": scalar output not possible")
  }
  c <- readsym$val[1,1]
  attr(c,"symName") <- readsym$name
  if (ts) {
    attr(c,"ts") <- readsym$ts
  }
  return(c)
} # rgdx.scalar

rgdx.set <- function(gdxName, symName, names=NULL, compress=FALSE, ts=FALSE)
{
  sym <- rgdx(gdxName, list(name=symName,compress=compress,ts=ts))
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
  attr(symDF,"symName") <- sym$name
  if (ts) {
    attr(symDF,"ts") <- sym$ts
  }
  return(symDF)
} # rgdx.set

# wgdx.df <- function(gdxName, df, df2=NULL, df3=NULL, df4=NULL)
wgdx.df <- function(gdxName, df)
{
  if (! is.character(gdxName)) {
    stop ("gdxName must GDX file name")
  }
  if (! is.data.frame(df)) {
    stop ("df must be a data frame")
  }
  symName <- attr(df, "symName", exact=TRUE)
  if (! is.character(symName)) {
    stop ("df must be a data frame with a character symName attribute")
  }
  nr <- nrow(df)
  nc <- ncol(df)
  isSet <- TRUE
  if (! is.factor(df[[1]])) {
    stop ("df[[1]] must be a factor")
  }
  for (j in c(2:nc-1)) {
    if (! is.factor(df[[j]])) {
      stop ("df[[", j, "]] must be a factor")
    }
  }
  if (is.factor(df[[nc]])) {
    symType <- "set"
    symDim <- nc
  }
  else if (is.numeric(df[[nc]])) {
    symType <- "parameter"
    isSet <- FALSE
    symDim <- nc-1
  }
  lst <- list (name=symName, type=symType, dim=symDim, form="sparse")
  symText <- attr(df, "ts", exact=TRUE)
  if (is.character(symText)) {
    lst$ts <- symText
  }
  v <- matrix(0, nrow=nr, ncol=nc)
  uels <- c()
  if (! isSet) {
    v[,symDim+1] <- df[,symDim+1]
  }
  for (j in c(1:symDim)) {
    v[,j] <- as.numeric(df[,j])
    uels <- c(uels,list(levels(df[,2])))
  }
  lst$val <- v
  lst$uels <- uels
  wgdx (gdxName, lst)
} # wgdx.df

wgdx.scalar <- function(gdxName, s)
{
  if (! is.character(gdxName)) {
    stop ("gdxName must GDX file name")
  }
  if (! is.numeric(s)) {
    stop ("s must be a scalar")
  }
  if (! is.null(dim(s))) {
    stop ("s must be a scalar")
  }
  symName <- attr(s, "symName", exact=TRUE)
  if (! is.character(symName)) {
    stop ("s must be a scalar with a character symName attribute")
  }
  lst <- list (name=symName, type="parameter", dim=0, form="full", val=as.numeric(s))
  symText <- attr(s, "ts", exact=TRUE)
  if (is.character(symText)) {
    lst$ts <- symText
  }
  wgdx (gdxName, lst)
}
