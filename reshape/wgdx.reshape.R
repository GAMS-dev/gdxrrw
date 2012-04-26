# given an input dataframe inDF (assumed to be a parameter),
# write the reshaped parameter to a GDX file
wgdx.reshape <- function(gdxName, inDF, parName, symDim, tName=NULL) {
  nCols <- ncol(inDF)
  inNames <- names(inDF)
  idCols <- 1:(symDim-1)
  dtCols <- symDim:nCols

  outDF <- reshape (inDF, idvar=inNames[idCols], varying=list(dtCols),
                    direction="long", times=inNames[dtCols])
  for (i in 1:symDim) {
    outDF[[i]] <- as.factor(outDF[[i]])
  }
  attr(outDF,"symName") <- parName
  if (is.character(tName)) {
    names(outDF)[symDim] <- tName
  }
  names(outDF)[symDim+1] <- "value"
  wgdx.df(gdxName,outDF)
} # wgdx.reshape
