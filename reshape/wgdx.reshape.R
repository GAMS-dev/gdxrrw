# write a reshaped parameter to GDX
#
# given an input dataframe inDF, reshape the data
# to make it suitable for input to wgdx.df.
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
                          gdxName=NULL, setsToo=TRUE, order=NULL) {
  nCols <- ncol(inDF)
  inNames <- names(inDF)
  if (is.null(order)) {
    idCols <- 1:(symDim-1)
    dtCols <- symDim:nCols
    str(idCols)
    outDF <- reshape (inDF, idvar=inNames[idCols], varying=list(dtCols),
                      direction="long", times=inNames[dtCols])
  }
  else if ((! is.vector(order)) || (symDim != length(order))) {
    stop ("specified order must be a vector of length symDim")
  }
  else {
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
        tCol <- k
      }
    }                                   # for k in 1:symDim
    if ((symDim-1) != idCount) {
      stop ('order vector must specify symDim-1 index columns')
    }
    oo <- c(idCols,(1:nCols)[-idCols])
    df2 <- inDF[oo]
    idCols <- 1:(symDim-1)
    dtCols <- symDim:nCols
    if (symDim == tCol) {               # no need to re-order after reshaping
      outDF <- reshape (df2, idvar=inNames[idCols], varying=list(dtCols),
                        direction="long", times=inNames[dtCols])
    }
    else {
      df3 <- reshape (df2, idvar=inNames[idCols], varying=list(dtCols),
                      direction="long", times=inNames[dtCols])
      oo <- c(1:(jCol-1),symDim,jCol:(symDim-1))
      outDF <- df3[oo]
    }
  }

  for (i in 1:symDim) {
    outDF[[i]] <- as.factor(outDF[[i]])
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
    names(outDF)[symDim] <- tName
  }
  else {
    names(outDF)[symDim] <- 'time'
  }
  names(outDF)[symDim+1] <- "value"
  str(outDF)
  if (setsToo) {
    ## write index sets first, then symName
    outLst <- list()
    length(outLst) <- symDim + 1
    for (i in 1:symDim) {
      lvls <- levels(outDF[[i]])
      outLst[[i]] <- list(name=names(outDF)[[i]], type='set', uels=c(list(lvls)))
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
      wgdx.df(gdxName,outDF)
    }
    else {
      return(list(outDF))
    }
  }
} # wgdx.reshape
