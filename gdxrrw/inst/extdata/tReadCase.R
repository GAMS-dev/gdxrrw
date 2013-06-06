#### test rgdx case-insensitivity in handling of uel input filter

if (! require(gdxrrw))      stop ("gdxrrw package is not available")
if (0 == igdx(silent=TRUE)) stop ("the gdx shared library has not been loaded")

errFunc <- function(ex) {
  print ("test of rgdx case-insensitivity in handling of uel input filter failed");
  print(ex)
  FALSE
} # errFunc

tryCatch({
  print ("testing rgdx case-insensitivity in handling of uel input filter")
  wgdx('?')
  fnIn <- "trnsport.gdx"
  if (! file_test ('-f', fnIn)) {
    stop (paste("FAIL: File", fnIn, "does not exist"))
  }

  uLst <- rgdx(fnIn)
  iUels <- uLst$uels
  loUels <- tolower(iUels)
  upUels <- toupper(iUels)
  iLst  <- rgdx(fnIn,list(name='i'))
  loLst <- rgdx(fnIn,list(name='i',uels=list(loUels)))
  upLst <- rgdx(fnIn,list(name='i',uels=list(upUels)))
  if (! identical(iLst$val,loLst$val)) {
    stop ("rgdx error: unexpected return in filtered read using loUels")
  }
  if (! identical(iLst$val,upLst$val)) {
    stop ("rgdx error: unexpected return in filtered read using upUels")
  }

  print ("test of rgdx case-insensitivity in handling of uel input filter passed")
  TRUE   ## all tests passed: return TRUE
},

error = errFunc
)
