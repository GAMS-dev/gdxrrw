#### test rgdx case-insensitivity in handling of uel input filter

if (! require(gdxrrw))      stop ("gdxrrw package is not available")
if (0 == igdx(silent=TRUE)) stop ("the gdx shared library has not been loaded")

testName <- 'case-insensitivity in handling of uel input filter'

errFunc <- function(ex) {
  print (paste0("test of rgdx on ",testName,": FAILED"))
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
  ## OK, the vals should be the same, no matter the case of the uels

  ## now check that the euls are really different
  if (identical(loUels,upUels)) {
    stop ("rgdx error: unexpected identical uels")
  }
  ## and that they match what comes back in the list
  if (! identical(loUels,loLst$uels[[1]])) {
    stop ("rgdx error: unexpected return in filtered read using loUels")
  }
  if (! identical(upUels,upLst$uels[[1]])) {
    stop ("rgdx error: unexpected return in filtered read using upUels")
  }


  print (paste0("test of rgdx on ", testName, ": PASSED"))
  invisible(TRUE)   ## all tests passed: return TRUE
},

error = errFunc
)
