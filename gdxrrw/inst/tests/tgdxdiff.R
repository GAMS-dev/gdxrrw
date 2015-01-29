### Test that the gdxdiff utility can be run successfully via system2()

if (! require(gdxrrw))      stop ("gdxrrw package is not available")
if (0 == igdx(silent=TRUE)) stop ("the gdx shared library has not been loaded")

fnData <- "trnsport.gdx"
logFile <- 'diffLog.txt'

errFunc <- function(ex) {
  print (paste("Check file", logFile, "for possible gdxdiff output"))
  print (ex)
  FALSE
} # errFunc

tryCatch({
  if (! file_test ('-f', fnData)) {
    stop (paste("FAIL: File", fnData, "does not exist"))
  }
  rc <- system2 ("gdxdiff",args=c(fnData, fnData), stdout=logFile)
  if (0 != rc) {
    stop(paste("Bad return from trivial gdxdiff: wanted 0, got",rc))
  } else {
    print ("gdxdiff call succeeded: it must be available and functional")
  }

  suppressWarnings(file.remove(logFile))
  invisible(TRUE)   ## all tests passed: return TRUE
},

error = errFunc
)
