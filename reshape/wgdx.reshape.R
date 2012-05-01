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
  idCols <- 1:(symDim-1)
  dtCols <- symDim:nCols

  outDF <- reshape (inDF, idvar=inNames[idCols], varying=list(dtCols),
                    direction="long", times=inNames[dtCols])
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
    stop ('setsToo true branch not implemented')
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
