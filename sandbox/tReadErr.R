#### test rgdx errors: some things should be an error,
#### and we expect certain behavior in these cases

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

  msg <- "rgdx test with bogus dim specifier"
  tcr <- tryCatch({
    IJ <- rgdx(fnIn,list(name='IJ',dim=4,form='sparsejj')) ; FALSE
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
