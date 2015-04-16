#### test rgdx with a file generated prior to domain info in GDX
#### test form=['sparse','full'] X [filtered,unfiltered] X compress=[T,F]

#### wanted lists produced with    dump("listName",file="")

if (! require(gdxrrw))      stop ("gdxrrw package is not available")
if (0 == igdx(silent=TRUE)) stop ("the gdx shared library has not been loaded")

source ("chkSame.R")
reqIdent <- TRUE

tryCatch({
  print ("testing rgdx handling of GDX file with no domain info")
  rgdx('?')
  fnIn <- "teTestPreDomainInfo.gdx"
  if (! file_test ('-f', fnIn)) {
    stop (paste("FAIL: File", fnIn, "does not exist"))
  }
  useDomInfo <- TRUE
  noInfo <- "none"   # domInfo="none" means we checked, but none is available

  source ("tReadDomInfoBody.R")


  print ("test of rgdx on pre-domain-info GDX file passed")
  TRUE   ## all tests passed: return TRUE
},

error = function(ex) { print ("test of rgdx on pre-domain-info GDX file failed"); print(ex) ; FALSE }
)
