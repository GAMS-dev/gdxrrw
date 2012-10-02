#### test rgdx and compress=TRUE and useDomInfo=FALSE with a recent GDX file containing full domain info
#### test both form=['sparse','full']

source ("chkSame.R")

tryCatch({
  print ("testing rgdx handling of GDX file with compress=TRUE and useDomInfo=FALSE")
  rgdx('?')
  fnIn <- "compressTest.gdx"
  if (! file_test ('-f', fnIn)) {
    stop (paste("FAIL: File", fnIn, "does not exist"))
  }
  useDomInfo <- FALSE                   # NOT the default

  source ("tReadCompressBody.R")


  print ("test of rgdx with compress=TRUE and useDomInfo=FALSE passed")
  TRUE   ## all tests passed: return TRUE
},

error = function(ex) { print ("test of rgdx with compress=TRUE and useDomInfo=FALSE failed"); print(ex) ; FALSE }
)
