#### test rgdx errors: some things should be an error,
#### and we expect certain behavior in these cases

tryCatch({
  print ("testing error detection and handling for rgdx")
  rgdx('?')
  fnIn <- "teTest.gdx"
  if (! file_test ('-f', fnIn)) {
    stop (paste("FAIL: File", fnIn, "does not exist"))
  }
  tcr <- tryCatch({
    IJ <- rgdx(fnIn,list(name='IJ',form='FFfull_invalid',compress=TRUE)) ; return(FALSE)
    },
    error = function(e) { print(e) ; return (TRUE) }
  )
  print(paste("bad call detected error:", tcr))
  if (! tcr) {
    return (FALSE)
  }

  return (TRUE)
},

error = function(ex) { print(ex) ; return (FALSE) }
)
