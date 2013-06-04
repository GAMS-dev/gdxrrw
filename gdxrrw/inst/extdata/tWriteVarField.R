#### test wgdx for handling of 'field' list element

if (! require(gdxrrw))      stop ("gdxrrw package is not available")
if (0 == igdx(silent=TRUE)) stop ("the gdx shared library has not been loaded")

errFunc <- function(ex) {
  print ("test of wgdx on 'field' list element for variable writes failed");
  print(ex)
  FALSE
} # errFunc

tryCatch({
  print ("testing wgdx on 'field' list element for variable writes")
  wgdx('?')
  fnIn <- "var2d.gdx"
  fnOut <- "tmp.gdx"
  if (! file_test ('-f', fnIn)) {
    stop (paste("FAIL: File", fnIn, "does not exist"))
  }

  xIn <- rgdx(fnIn,list(name='x',form='sparse',field='all'))

  ## test field='all'
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

  ## test empty field, identical to 'all'
  x2 <- xIn
  x2$field <- NULL
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

  msg <- "wgdx test for variable with bogus field specifier"
  x2$field <- 'notGood'
  tcr <- tryCatch({
    wgdx(fnOut, x2) ; FALSE
    },
    error = function(e) { print(paste(' Caught error: msg =',e)) ; TRUE }
  )
  if (tcr) {
    print(paste(msg,": passed",sep=""))
  }
  else {
    stop (paste(msg, ": failed",sep=""))
  }


  print ("test of wgdx on 'field' list element for variable passed")
  TRUE   ## all tests passed: return TRUE
},

error = errFunc
)
