# how to introduce new syntax for rgdx?

rCurr <- function(gdxName, requestList = NULL, squeeze=TRUE, useDomInfo=TRUE)
{
  if (is.null(requestList) && (gdxName == '?')) {
    invisible(.External(rgdxExt, gdxName=gdxName, requestList=NULL,
                        squeeze=squeeze, useDomInfo=useDomInfo))
  }
  else {
    .External(rgdxExt, gdxName=gdxName, requestList=requestList,
              squeeze=squeeze, useDomInfo=useDomInfo)
  }
} # rCurr

# rNew: new interface, no attempt at backward compatibility
rNew <- function(gdxName, symName = NULL, squeeze=TRUE, useDomInfo=TRUE)
{
  if (is.null(symName) && (gdxName == '?')) {
    return(invisible(.External(rgdxExt, gdxName=gdxName, requestList=NULL,
                        squeeze=squeeze, useDomInfo=useDomInfo)))
  }
  if (is.null(symName)) {
    return(.External(rgdxExt, gdxName=gdxName, requestList=NULL,
                     squeeze=squeeze, useDomInfo=useDomInfo))
  }
  rr <- list()
  if (! is.null(symName)) {
    rr$name <- symName
  }
  return(.External(rgdxExt, gdxName=gdxName, requestList=rr,
                   squeeze=squeeze, useDomInfo=useDomInfo))
} # rNew
