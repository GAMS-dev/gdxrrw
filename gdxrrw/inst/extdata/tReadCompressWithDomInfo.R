#### test rgdx and compress=TRUE with a recent GDX file containing full domain info
#### test both form=['sparse','full']

source ("chkSame.R")

tryCatch({
  print ("testing rgdx handling of GDX file with compress=TRUE")
  rgdx('?')
  fnIn <- "compressTest.gdx"
  if (! file_test ('-f', fnIn)) {
    stop (paste("FAIL: File", fnIn, "does not exist"))
  }
  compress <- TRUE
  useDomInfo <- TRUE                    # the default

  source ("tReadCompressBody.R")


  print ("test of rgdx with compress=TRUE passed")
  TRUE   ## all tests passed: return TRUE
},

error = function(ex) { print ("test of rgdx with compress=TRUE failed"); print(ex) ; FALSE }
)
