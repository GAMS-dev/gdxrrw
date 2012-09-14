#### test rgdx errors: some things should be an error,
#### and we expect certain behavior in these cases

tryCatch({
  print ("testing error detection and handling for rgdx")
  rgdx('?')
  fnIn <- "teTest.gdx"
  if (! file_test ('-f', fnIn)) {
    stop (paste("FAIL: File", fnIn, "does not exist"))
  }
  foundErr <- FALSE
  tcr <- tryCatch({IJ <- rgdx(fnIn,list(name='IJ',form='FFfull',compress=TRUE)) ; return(TRUE)},
                  error = function(e) { print(e); foundErr <- TRUE; print("Done with inner") ; return (FALSE)})
  print(paste("foundErr =", foundErr))
  print(tcr)
#  IJ <- rgdx(fnIn,list(name='IJ',form='FFfull',compress=TRUE))

  return (TRUE)
},

error = function(ex) { print(ex) ; return (FALSE) }
)
