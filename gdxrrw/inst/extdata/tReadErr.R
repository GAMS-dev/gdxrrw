#### test rgdx errors: some things should be an error,
#### and we expect certain behavior in these cases

if (! require(gdxrrw))      stop ("gdxrrw package is not available")
if (0 == igdx(silent=TRUE)) stop ("the gdx shared library has not been loaded")

tryCatch({
  print ("testing error detection and handling for rgdx")
  rgdx('?')
  fnIn <- "teTest.gdx"
  if (! file_test ('-f', fnIn)) {
    stop (paste("FAIL: File", fnIn, "does not exist"))
  }

  msg <- "rgdx test with bogus form specifier"
  tcr <- tryCatch({
    IJ <- rgdx(fnIn,list(name='IJ',form='invalidForm',compress=TRUE)) ; FALSE
    },
    error = function(e) { print(paste(' Caught error: msg =',e)) ; TRUE }
  )
  if (tcr) {
    print(paste(msg,": passed",sep=""))
  }
  else {
    stop (paste(msg, ": failed",sep=""))
  }

  # set IJ has dimension 2, not 4
  msg <- "rgdx test with bogus dim specifier"
  tcr <- tryCatch({
    IJ <- rgdx(fnIn,list(name='IJ',dim=4)) ; FALSE
    },
    error = function(e) { print(paste(' Caught error: msg =',e)) ; TRUE }
  )
  if (tcr) {
    print(paste(msg,": passed",sep=""))
  }
  else {
    stop (paste(msg, ": failed",sep=""))
  }


  # parameters do not have text elements
  msg <- "rgdx test with specifier te=TRUE for a parameter"
  tcr <- tryCatch({
    IJ <- rgdx(fnIn,list(name='A',te=TRUE)) ; FALSE
    },
    error = function(e) { print(paste(' Caught error: msg =',e)) ; TRUE }
  )
  if (tcr) {
    print(paste(msg,": passed",sep=""))
  }
  else {
    stop (paste(msg, ": failed",sep=""))
  }
  ## all tests passed: return TRUE
  print ("test of rgdx error handling: all passed")
  TRUE
},

error = function(ex) { print ("test of rgdx error handling failed"); print(ex) ; FALSE }
)
