## testing new argument convention for rgdx()
##
## old-style:
##   rgdx <- function(gdxName, requestList = NULL, squeeze=TRUE, useDomInfo=TRUE)
##   where requestList contains what are essentially function args (e.g. name, form, uels)
##
## better way:
##   rgdx <- function(gdxName,
##                    symName = NULL,
##                    form = 'sparse',
##                    uels = NULL,
##                    ts = FALSE,
##                    te = FALSE,
##                    field = NULL,
##                    dim = NULL,
##                    compress = FALSE,
##                    squeeze=TRUE, useDomInfo=TRUE
## 
##
## How can we do both at once?

rrgdx <- function(gdxName, ..., squeeze=TRUE, useDomInfo=TRUE)
{
  ## for empty dots and gdxName == '?', it is an audit run
  dotList <- list(...)
  nDots <- length(dotList)
  print (paste("nDots =", nDots))
  if (0 == nDots) {
    if ('?' == gdxName) {
      return(invisible(.External(gdxrrw:::rgdxExt, gdxName='?', requestList=NULL,
                                 squeeze=squeeze, useDomInfo=useDomInfo)))
    } else {
      return(.External(gdxrrw:::rgdxExt, gdxName=gdxName, requestList=NULL,
                       squeeze=squeeze, useDomInfo=useDomInfo))
    }
  }
  print ("We did not take the audit path or empty path")

  ## which way to go, old or new style?
  ## two cases: names exist for some args, or for no args

  dotNames <- names(dotList)
  if (is.null(dotNames)) {
    print ("no dotNames provided")
    stop ("not yet implemented")
  } else {
    ## if any arg is called "requestList",
    ##   it must be the only arg in dotNames, and we are an old-style call
    ## else
    ##   we are a new-style call, all args should have names
    isOldStyle <- FALSE
    for (k in c(1:nDots)) {
      print (paste("  arg",k,"  name:", dotNames[k]))
      if ('requestList' == dotNames[k])
        isOldStyle == TRUE
      # print (names(substitute(dotList[[k]])))
      # print (deparse(substitute(dotList[[k]])))
      # print (deparse(substitute(dotList)))
      # print (names(substitute(dotList))[k])

    }
    print(paste("isOldStyle:",isOldStyle))
    stop("with-names-handling not yet done")
  }



  print (paste("dotNames = ",dotNames))
  print (deparse(dotList))

  print (deparse(substitute(dotList)))
  for (k in c(1:nDots)) {
    print (paste("  dot-arg",k,"  name:", "unknown"))
    # print (names(substitute(dotList[[k]])))
    # print (deparse(substitute(dotList[[k]])))
    # print (deparse(substitute(dotList)))
    print (names(substitute(dotList))[k])
 }

  ## if (is.null(requestList) && (gdxName == '?')) {
  ##   invisible(.External(rgdxExt, gdxName=gdxName, requestList=NULL,
  ##                       squeeze=squeeze, useDomInfo=useDomInfo))
  ## }
  ## else {
  ##   .External(rgdxExt, gdxName=gdxName, requestList=requestList,
  ##             squeeze=squeeze, useDomInfo=useDomInfo)
  ## }
}

