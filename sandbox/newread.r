## testing new argument convention for rgdx()
##
## old-style:
##   rgdx <- function(gdxName, requestList = NULL, squeeze=TRUE, useDomInfo=TRUE)
##   where requestList contains what are essentially function args (e.g. name, form, uels)
##
## desired way (tabula rasa or wishful thinking, not necessarily back-compatible):
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
## How can we do both at once, or close to both,
## and move to something close to the desired way?


## rrgdx: we could re-implement using the ... as a waypoint, and move to the desired way
## once we stop accepting the old syntax
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
    if ((1 == nDots) && is.list(dotList[[1]])) {
      ## single '...' arg, in list form: assume old-style call
      return(.External(gdxrrw:::rgdxExt, gdxName=gdxName, requestList=dotList[[1]],
                       squeeze=squeeze, useDomInfo=useDomInfo))
    }
    else {
      ## assume new-style call, pop ... args into a named list for now
      ## to mimic how they would get named in the desired rgdx above
      if (nDots > 8) {
        stop ("incorrect '...' args: more than 8 args specified")
      }
      names(dotList) <- c("name", "form","uels","ts","te","field","dim","compress")[1:nDots]
      print(dotList)
      return(.External(gdxrrw:::rgdxExt, gdxName=gdxName, requestList=dotList,
                       squeeze=squeeze, useDomInfo=useDomInfo))
    }
    stop ("no-dotNames handling not yet implemented")
  } else {
    ## if any arg is called "requestList",
    ##   it must be the only arg in dotNames, and we are an old-style call
    ## else
    ##   we are a new-style call, all args should have names
    isOldStyle <- FALSE
    for (k in c(1:nDots)) {
      print (paste("  arg",k,"  name:", dotNames[k]))
      if ("requestList" == dotNames[k])
        isOldStyle <- TRUE
      # print (names(substitute(dotList[[k]])))
      # print (deparse(substitute(dotList[[k]])))
      # print (deparse(substitute(dotList)))
      # print (names(substitute(dotList))[k])
    }
    if (isOldStyle) {
      ## named arg "requestList" found, so expect just a single arg
      if (1 != nDots) {
        stop ("incorrect '...' args: if 'requestList' arg exists, it must be the only '...' arg")
      }
      return(.External(gdxrrw:::rgdxExt, gdxName=gdxName, requestList=dotList[[k]],
                       squeeze=squeeze, useDomInfo=useDomInfo))
    }
    else {
      print(paste("isOldStyle:",isOldStyle))
      ## the commented lines below are how we'd call the updated external
      ## return(.External(gdxrrw:::rgdxExt, gdxName=gdxName, ...,
      ##                  squeeze=squeeze, useDomInfo=useDomInfo))
      return(.External(gdxrrw:::rgdxExt, gdxName=gdxName, dotList,
                       squeeze=squeeze, useDomInfo=useDomInfo))
    }
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

## -------------------------------------------------
## # some example calls to catch as old-style:
## rgdx('t.gdx',list(name='I'))
## rgdx('t.gdx',list(name='I'), FALSE, FALSE)
## rgdx('t.gdx',list(name='I'), FALSE)
## rgdx('t.gdx',requestList=list(name='I'))
## rgdx('t.gdx',requestList=list(name='I'), FALSE, FALSE)
## rgdx('t.gdx',requestList=list(name='I'), useDomInfo=FALSE, TRUE)
## -------------------------------------------------

## or we could just go for the new syntax immediately.
## we can do some really dirty stuff to recognize an old call
## how do we allow for the named arg requestList=list(name='I') though?
xrgdx <- function(gdxName,
                  symName = NULL,
                  form = 'sparse',
                  uels = NULL,
                  ts = FALSE,
                  te = FALSE,
                  field = NULL,
                  dim = NULL,
                  compress = FALSE,
                  squeeze=TRUE, useDomInfo=TRUE, ...)
{
  ## what gets shoved into the dots?
  if (missing(gdxName)) {
    print ("gdxName arg is missing")
  }
  else {
    print (paste("gdxName =", gdxName))
  }
  print (paste("symName =", symName))
  dotList <- list(...)
  nDots <- length(dotList)
  dotNames <- names(dotList)
  print (paste("nDots =", nDots))
  if (0 == nDots) {
    print ("no extra args, so assume no named requestList arg")
    if (is.list(symName)) {
      print ("symName is a list: old-style processing")
      ## form and uels are not named args in valid old-style calls
      if (! missing(form)) {
        if (missing(squeeze)) {
          squeeze <- form
        }
        else if (missing(useDomInfo)) {
          useDomInfo <- form
        }
        else {
          stop ("invalid arguments: too many args specified")
        }
      }
      if (! missing(uels)) {
        if (missing(useDomInfo)) {
          useDomInfo <- uels
        }
        else {
          stop ("invalid arguments: too many args specified")
        }
      }
      print(paste("missing   (squeeze) =",missing(squeeze)))
      print(paste("missing(useDomInfo) =",missing(useDomInfo)))
      return(.External(gdxrrw:::rgdxExt, gdxName=gdxName, symName,
                       squeeze=squeeze, useDomInfo=useDomInfo))
    }
    else {
      print ("symName is a not list: new-style processing")
      return(.External(gdxrrw:::rgdxExt, gdxName=gdxName,
                       list(name = symName,
                            form = form,
                            uels = uels,
                            ts = ts,
                            te = te,
                            field = field,
                            dim = dim,
                            compress = compress),
                       squeeze=squeeze, useDomInfo=useDomInfo))
    }
    return(NULL)
  }

  if (is.null(dotNames)) {
    stop ("invalid arguments: unused/extra args must be named")
  }
  if ((1 != nDots) || ("requestList" != dotNames[1])) {
    stop ("invalid arguments: only one extra arg 'requestList=xxx' allowed")
  }
  if (! is.list(dotList[[1]])) {
    stop ("invalid arguments: extra arg 'requestList' is not a list")
  }
  ## we assume any old-style call is a CORRECT call,
  ## so symName and form are not named args if requestList is a named arg
  if (! missing(symName)) {
    if (missing(squeeze)) {
      squeeze <- symName
    }
    else if (missing(useDomInfo)) {
      useDomInfo <- symName
    }
    else {
      stop ("invalid arguments: too many args specified")
    }
  }
  if (! missing(form)) {
    if (missing(useDomInfo)) {
      useDomInfo <- form
    }
    else {
      stop ("invalid arguments: too many args specified")
    }
  }
  print(paste("missing   (squeeze) =",missing(squeeze)))
  print(paste("missing(useDomInfo) =",missing(useDomInfo)))
  return(.External(gdxrrw:::rgdxExt, gdxName=gdxName, dotList[[1]],
                   squeeze=squeeze, useDomInfo=useDomInfo))
} # xrgdx
