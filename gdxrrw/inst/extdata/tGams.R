#### test basic functionality of gams() function

tryCatch({
  print ("basic testing of gams() function")
  gams('?')

  msg <- "gams test with nonexistent file"
  tcr <- tryCatch({
    rc <- gams("notThere.gms") ; FALSE
    },
    error = function(e) { print(paste(' Caught error: msg =',e)) ; TRUE }
  )
  if (tcr) {
    print(paste(msg,": passed",sep=""))
  }
  else {
    stop (paste(msg, ": failed",sep=""))
  }

  ## rc <- gams("notThere.gms")
  ## if (2 != rc) {
  ##  stop (paste("gams('notThere.gms') returned",rc,"expected 2"))
  ## }

  cat ("scalar a / 1 /\n", file="tmp.gms");
  rc <- gams("tmp.gms")
  if (0 != rc) {
    stop (paste("valid gams source: gams('tmp.gms') returned",rc,"expected 0"))
  }

  cat ("skalar a / 1 /\n", file="tmp.gms");
  rc <- gams("tmp.gms")
  if (2 != rc) {
    stop (paste("invalid gams source: gams('tmp.gms') returned",rc,"expected 2"))
  }

  print ("basic gams() tests passed")
  TRUE   ## all tests passed: return TRUE
},

error = function(ex) { print ("basic gams() tests failed"); print(ex) ; FALSE }
)
