#### test rgdx with a recent GDX file containing full domain info
#### with useDomInfo=FALSE it should behave as if no dom info is there
#### test form=['sparse','full'] X [filtered,unfiltered] X compress=[T,F]

#### wanted lists produced with    dump("listName",file="")

if (! require(gdxrrw))      stop ("gdxrrw package is not available")
if (0 == igdx(silent=TRUE)) stop ("the gdx shared library has not been loaded")

source ("chkSame.R")
reqIdent <- TRUE

tryCatch({
  print ("testing rgdx handling of GDX file with useDomInfo=FALSE")
  rgdx('?')
  fnIn <- "teTestWithDomainInfo.gdx"
  if (! file_test ('-f', fnIn)) {
    stop (paste("FAIL: File", fnIn, "does not exist"))
  }
  useDomInfo <- FALSE

  source ("tReadDomInfoBody.R")


  print ("test of rgdx with useDomInfo=FALSE passed")
  TRUE   ## all tests passed: return TRUE
},

error = function(ex) { print ("test of rgdx with useDomInfo=FALSE failed"); print(ex) ; FALSE }
)
