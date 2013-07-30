
rgdx <- function(gdxName, requestList = NULL, squeeze=TRUE, useDomInfo=TRUE)
{
  if (is.null(requestList) && (gdxName == '?')) {
    invisible(.External(rgdxExt, gdxName=gdxName, requestList=NULL,
                        squeeze=squeeze, useDomInfo=useDomInfo))
  }
  else {
    .External(rgdxExt, gdxName=gdxName, requestList=requestList,
              squeeze=squeeze, useDomInfo=useDomInfo)
  }
}

wgdx <- function(gdxName, ..., squeeze='y')
{
  invisible(.External(wgdxExt, gdxName=gdxName, ..., squeeze=squeeze))
}

gams <- function(gmsAndArgs)
{
  .External(gamsExt, gmsAndArgs)
}

gdxInfo <- function(gdxName = NULL, dump=TRUE, returnList=FALSE, returnDF=FALSE)
{
  d <- as.logical(dump)
  if (is.na(d)) {
    stop ("gdxInfo: argument dump=", print(dump), " not a good logical value")
  }
  rl <- as.logical(returnList)
  if (is.na(rl)) {
    stop ("gdxInfo: argument returnList=", print(returnList), " not a good logical value")
  }
  rdf <- as.logical(returnDF)
  if (is.na(rdf)) {
    stop ("gdxInfo: argument returnDF=", print(returnDF), " not a good logical value")
  }
#  print (paste('gdxInfo: dump=',d,'returnList=',rl,'returnDF=',rdf))
  if (! (rl || rdf)) {
    invisible(.External(gdxInfoExt, gdxName=gdxName, dump=d, returnList=rl,
                        returnDF=rdf))
  }
  else {
    .External(gdxInfoExt, gdxName=gdxName, dump=d, returnList=rl,
              returnDF=rdf)
  }
} # gdxInfo

igdx <- function(gamsSysDir = NULL, silent = FALSE, returnStr = FALSE)
{
  invisible(.External(igdxExt, gamsSysDir, silent=silent, returnStr=returnStr))
}

rgdx.param <- function(gdxName, symName, names=NULL, compress=FALSE, ts=FALSE, squeeze=TRUE, useDomInfo=TRUE)
{
  sym <- rgdx(gdxName, list(name=symName,compress=compress,ts=ts),squeeze=squeeze,useDomInfo=useDomInfo)
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
  attr(symDF,"domains") <- sym$domains
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
  c <- 0
  if (1 == dim(readsym$val)[1]) {
    c <- readsym$val[1,1]
  }
  attr(c,"symName") <- readsym$name
  if (ts) {
    attr(c,"ts") <- readsym$ts
  }
  return(c)
} # rgdx.scalar

rgdx.set <- function(gdxName, symName, names=NULL, compress=FALSE, ts=FALSE, useDomInfo=TRUE)
{
  sym <- rgdx(gdxName, list(name=symName,compress=compress,ts=ts), useDomInfo=useDomInfo)
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
  attr(symDF,"domains") <- sym$domains
  if (ts) {
    attr(symDF,"ts") <- sym$ts
  }
  return(symDF)
} # rgdx.set

## processScalar: process a scalar given as input to wgdx
## output a list suitable for raw wgdx
processScalar <- function(s, msg)
{
  symName <- attr(s, "symName", exact=TRUE)
  if (! is.character(symName)) {
    stop ("error processing ", msg, ": missing/bogus symName attribute")
  }
  o <- list (name=symName, type="parameter", dim=0, form="full", val=as.numeric(s))
  symText <- attr(s, "ts", exact=TRUE)
  if (is.character(symText)) {
    o$ts <- symText
  }
  o
} # processScalar

## processDF: process a dataframe given as input to wgdx
## output a list suitable for raw wgdx
processDF <- function(df, msg)
{
  symName <- attr(df, "symName", exact=TRUE)
  if (! is.character(symName)) {
    stop ("error processing ", msg, ": missing/bogus symName attribute")
  }
  nr <- nrow(df)
  nc <- ncol(df)
  isSet <- TRUE
  if (! is.factor(df[[1]])) {
    stop ("(",msg,")[[1]] must be a factor")
  }
  for (j in 1 + seq_len(max(0,nc-2))) {
    if (! is.factor(df[[j]])) {
      stop ("(",msg,")[[", j, "]] must be a factor")
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
  o <- list (name=symName, type=symType, dim=symDim, form="sparse")
  symText <- attr(df, "ts", exact=TRUE)
  if (is.character(symText)) {
    o$ts <- symText
  }
  v <- matrix(0, nrow=nr, ncol=nc)
  uels <- c()
  if (! isSet) {
    v[,symDim+1] <- df[,symDim+1]
  }
  for (j in c(1:symDim)) {
    v[,j] <- as.numeric(df[,j])
    uels <- c(uels,list(levels(df[,j])))
  }
  o$val <- v
  o$uels <- uels
  o
} # processDF

# wgdx.lst: write multiple symbols to a GDX file
# the routines above write only one symbol to GDX, where the symbol info 
# takea a different form for each function:
#   wgdx.df    : input is a data frame containing a set or param
#   wgdx.scalar: input is a scalar
#   wgdx       : input is one or more lists, each list holds one symbol
# This routine accepts all three types of input, singly or in lists
# Let S be a data frame, scalar, or list-holding-a-symbol, as just mentioned
# Let L be a list of elements of type S.
# Each element of "..." for this function must be of type L or type S
wgdx.lst <- function(gdxName, ..., squeeze='y')
{
  if (! is.character(gdxName)) {
    stop ("bad gdxName: must be a GDX file name")
  }
  dotList <- list(...)
  nInputs <- length(dotList)
  olst <- list()
  kOut <- 0

  for (k in c(1:nInputs)) {
    item <- dotList[[k]]
    # print (paste("*** Processing item", k))
    if (is.data.frame(item)) {
      # print (" *** found a data frame")
      kOut <- kOut + 1
      olst[[kOut]] <- processDF (item, paste("argument",k+1))
    }
    else if (! is.list (item) &&
             is.numeric(item) &&
             is.null(dim(item)) ) {
      # reading a scalar
      # print (" *** found a scalar")
      kOut <- kOut + 1
      olst[[kOut]] <- processScalar (item, paste("argument",k+1))
    }
    else if (is.list (item)) {
      if (is.null(names(item))) {
        ## unnamed list: each element must be a dataframe, scalar, or symbol list
        # print (" *** found an unnamed list:")
        nList <- length(item)
        for (kk in c(1:nList)) {
          item2 <- item[[kk]]
          # print (paste("  *** Processing item2", kk))
          if (is.data.frame(item2)) {
            # print ("   *** found a data frame")
            kOut <- kOut + 1
            olst[[kOut]] <- processDF (item2, paste0("arg",k+1,"[[",kk,"]]"))
          }
          else if (! is.list (item2) &&
                   is.numeric(item2) &&
                   is.null(dim(item2)) ) {
                                        # reading a scalar
            # print ("   *** found a scalar")
            kOut <- kOut + 1
            olst[[kOut]] <- processScalar (item2, paste0("arg",k+1,"[[",kk,"]]"))
          }
          else if (is.list (item2)) {
            if (! is.null(names(item2))) {
              ## named list: add to the outputs
              kOut <- kOut + 1
              olst[[kOut]] <- item2
            }
            else {
              stop (paste0("invalid input found: arg",k+1,"[[",kk,"]] is a list but does not contain symbol info"))
            }
          }
          else {
            stop (paste0("unrecognized input found: arg",k+1,"[[",kk,"]] not valid"))
          }
        } # loop over kk: item2 members of item list
      }
      else {
        # print (" *** found a named list:")
        kOut <- kOut + 1
        olst[[kOut]] <- item
      }
    }
    else {
      stop (paste("unrecognized input found: argument",k+1,"not valid"))
    }
  }

  wgdx (gdxName, olst, squeeze=squeeze)
} # wgdx.lst

# write a reshaped parameter to GDX
#
# given an input dataframe inDF, reshape the data
# to make it suitable for input to wgdx.lst.
# The reshaped data will be for a parameter named symName of dimension symDim.
# The headers for the data columns of inDF will be combined in a new index
# position named tName.
# If no GDX name is given, the list that would have gone to GDX via
# gdx.lst is returned instead.  If a GDX name is given, no list is returned.
# By default, data frames for the sets defined by each of the index columns
# of the parameter are also written to the GDX container or returned.
# By default, the first (symDim-1) columns of inDF are assumed to represent
# the initial (symDim-1) indices of the parameter, while the remaining columns
# are data columns whose headers contain the indices for the rightmost index
# position of the parameter.  To select different columns of inDF
# as the index and data columns, or to reorder the columns of the output,
# use the order parameter.

# list output if no GDX
# TO DO:
#   what about set data?
#   check if setsToo default could be FALSE: ping Renger etc.

wgdx.reshape <- function (inDF, symDim, symName=NULL, tName="time",
                          gdxName=NULL, setsToo=TRUE, order=NULL,
                          setNames=NULL) {
  nCols <- ncol(inDF)
  timeIdx <- symDim                     # default index position for time aggregate
  if (is.null(order)) {
    idCols <- 1:(symDim-1)
    for (i in idCols) {
      inDF[[i]] <- as.factor(inDF[[i]])
    }
    outDF <- melt (inDF, id=idCols)
  }
  else if ((! is.vector(order)) || (symDim != length(order))) {
    stop ("specified order must be a vector of length symDim")
  }
  else {
    timeIdx <- -1
    if (is.character(order)) {
      stop ("order must be numeric for now")
    }
    else if (! is.numeric(order)) {
      stop ("optional order vector must be numeric or character")
    }
    idCols <- vector(mode="integer",length=symDim-1)
    dtCols <- 1:nCols
    idCount <- 0
    for (k in 1:symDim) {
      j <- order[k]
      if (j > 0) {
        idCount <- idCount + 1
        if (dtCols[j] <= 0) {
          stop ('duplicate entry in order vector: nonsense')
        }
        dtCols[j] <- 0
        idCols[idCount] <- j
      }
      else {
        timeIdx <- k
      }
    }                                   # for k in 1:symDim
    if ((symDim-1) != idCount) {
      stop ('order vector must specify symDim-1 index columns')
    }
    if ((symDim-1) != idCount) {
      stop ('order vector must have a non-positive entry to specify the "time" index')
    }
    oo <- c(idCols,(1:nCols)[-idCols])
    df2 <- inDF[oo]
    idCols <- 1:(symDim-1)
    for (i in idCols) {
      df2[[i]] <- as.factor(df2[[i]])
    }
    if (symDim == timeIdx) {     # no need to re-order after reshaping
      outDF <- melt (df2, id=idCols)
    }
    else {
      df3 <- melt (df2, id=idCols)
      oo <- vector(mode="integer",length=symDim+1)
      for (k in 1:timeIdx-1) {
        oo[k] = k
      }
      oo[timeIdx] = symDim
      for (k in timeIdx+1:symDim) {
        oo[k] = k-1
      }
      oo[symDim+1] = symDim+1
      outDF <- df3[oo]
    }
  }

  if (is.null(symName)) {
    symName <- attr(inDF, "symName", exact=TRUE)
  }
  if (! is.character(symName)) {
    stop ("symName must be a string")
  }
  attr(outDF,"symName") <- symName
  symText <- attr(inDF, "ts", exact=TRUE)
  if (is.character(symText)) {
    attr(outDF,"ts") <- symText
  }
  if (is.character(tName)) {
    names(outDF)[timeIdx] <- tName
  }
  else {
    names(outDF)[timeIdx] <- 'time'
  }
  names(outDF)[symDim+1] <- "value"
  # str(outDF)
  if (setsToo) {
    ## write index sets first, then symName
    outLst <- list()

    length(outLst) <- symDim + 1
    setNamesLen <- 0
    if (! is.null(setNames)) {
      if (! is.character(setNames)) {
        stop ("setNames must be a string or string vector")
      }
      else if (! is.vector(setNames)) {
        stop ("setNames must be a string or string vector")
      }
      else {
        ## guard against zero-length vector
        setNamesLen <- length(setNames)
      }
    }
    kk <- 0
    for (i in 1:symDim) {
      lvls <- levels(outDF[[i]])
      outLst[[i]] <- list(name=names(outDF)[[i]], type='set', uels=c(list(lvls)))
      if (setNamesLen > 0) {            # tack on the next set text
        kk <- kk + 1
        outLst[[i]]$ts <- setNames[[kk]]
        if (kk >= setNamesLen)
          kk <- 0
      }
    }
    outLst[[symDim+1]] <- outDF
    if (is.character(gdxName)) {
      wgdx.lst(gdxName,outLst)
    }
    else {
      return(outLst)
    }
  }
  else {
    if (is.character(gdxName)) {
      wgdx.lst(gdxName,outDF)
    }
    else {
      return(list(outDF))
    }
  }
} # wgdx.reshape

# typeCode constants for variables
GMS_VARTYPE <- list(UNKNOWN=0L,
                    BINARY=1L,
                    INTEGER=2L,
                    POSITIVE=3L,
                    NEGATIVE=4L,
                    FREE=5L,
                    SOS1=6L,
                    SOS2=7L,
                    SEMICONT=8L,
                    SEMIINT=9L,
                    MAX=10L)

# typeCode constants for equations
GMS_EQUTYPE <- list(E=0L,
                    G=1L,
                    L=2L,
                    N=3L,
                    X=4L,
                    C=5L,
                    MAX=6L)
