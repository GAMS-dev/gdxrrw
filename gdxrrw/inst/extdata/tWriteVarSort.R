#### test wgdx with form='sparse', 2-d variable, on sorted and un-sorted 'val'

if (! require(gdxrrw))      stop ("gdxrrw package is not available")
if (0 == igdx(silent=TRUE)) stop ("the gdx shared library has not been loaded")

testName <- 'sorted and unsorted variable writes'

errFunc <- function(ex) {
  print (paste0("test of wgdx on ",testName,": FAILED"))
  print(ex)
  FALSE
} # errFunc

tryCatch({
  print ("testing wgdx on sparse variable write, sorted and unsorted")
  wgdx('?')
  fnIn <- "var2d.gdx"
  fnOut <- "tmp.gdx"
  if (! file_test ('-f', fnIn)) {
    stop (paste("FAIL: File", fnIn, "does not exist"))
  }

  xIn <- rgdx(fnIn,list(name='x',form='sparse',field='all'))
  xIn

  wgdx (fnOut, xIn)
  if (file_test ('-f', fnOut) == TRUE) {
    print (paste("File", fnOut, "was created"))
  } else {
    stop (paste("FAIL: File", fnOut, "is not readable"))
  }
  rc <- system (paste("gdxdiff", fnIn, fnOut))
  if (0 != rc) {
    stop(paste("Bad return from gdxdiff: wanted 0, got",rc))
  } else {
    print ("gdxdiff call succeeded")
  }

  x2 <- xIn[c("name","type","dim","val","form","uels","typeCode")]
  wgdx (fnOut, x2)
  if (file_test ('-f', fnOut) == TRUE) {
    print (paste("File", fnOut, "was created"))
  } else {
    stop (paste("FAIL: File", fnOut, "is not readable"))
  }
  rc <- system (paste("gdxdiff", fnIn, fnOut))
  if (0 != rc) {
    stop(paste("Bad return from gdxdiff: wanted 0, got",rc))
  } else {
    print ("gdxdiff call succeeded")
  }

  x3 <- xIn[c("name","type","dim","form","uels","typeCode")]
  v <- xIn$val
  dv <- dim(v)
  nrow <- dv[1]
  ncol <- dv[2]
  vv <- matrix(nrow=nrow,ncol=ncol)
  for (i in 1:nrow) {
    iFrom <- nrow + 1 - i
    for (j in 1:ncol)
      vv[i,j] = v[iFrom,j]
  }
  x3$val <- vv
  wgdx (fnOut, x3)
  if (file_test ('-f', fnOut) == TRUE) {
    print (paste("File", fnOut, "was created"))
  } else {
    stop (paste("FAIL: File", fnOut, "is not readable"))
  }
  rc <- system (paste("gdxdiff", fnIn, fnOut))
  if (0 != rc) {
    stop(paste("Bad return from gdxdiff: wanted 0, got",rc))
  } else {
    print ("gdxdiff call succeeded")
  }


  print (paste0("test of wgdx on ", testName, ": PASSED"))
  invisible(TRUE)   ## all tests passed: return TRUE
},

error = errFunc
)
